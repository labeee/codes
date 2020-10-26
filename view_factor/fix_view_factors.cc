// FIRST FUNCTION

    void CalcApproximateViewFactors(int const N,                    // NUMBER OF SURFACES
                                    const Array1D<Real64> &A,       // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
                                    const Array1D<Real64> &Azimuth, // Facing angle of the surface (in degrees)
                                    const Array1D<Real64> &Tilt,    // Tilt angle of the surface (in degrees)
                                    Array2A<Real64> F,              // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
                                    const Array1D_int &SPtr         // pointer to REAL(r64) surface number (for error message)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Curt Pedersen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       March 2001 (RKS) to disallow surfaces facing the same direction to interact radiatively
        //                      May 2002 (COP) to include INTMASS, FLOOR, ROOF and CEILING.
        //       RE-ENGINEERED  September 2000 (RKS for EnergyPlus)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine approximates view factors using an area weighting.
        // This is improved by one degree by not allowing surfaces facing the same
        // direction to "see" each other.

        // METHODOLOGY EMPLOYED:
        // Each surface sees some area of other surfaces within the zone.  The view
        // factors from the surface1 to the other seen surfaces are defined by their
        // area over the summed area of seen surfaces.  Surfaces facing the same angle
        // are assumed to not be able to see each other.
        //  Modified May 2002 to cover poorly defined surface orientation.  Now all thermal masses, roofs and
        //  ceilings are "seen" by other surfaces. Floors are seen by all other surfaces, but
        //  not by other floors.

        // Argument array dimensioning
        EP_SIZE_CHECK(A, N);
        EP_SIZE_CHECK(Azimuth, N);
        EP_SIZE_CHECK(Tilt, N);
        F.dim(N, N);
        EP_SIZE_CHECK(SPtr, N);

        // Locals
        // SUBROUTINE ARGUMENTS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SameAngleLimit(10.0); // If the difference in the azimuth angles are above this value (degrees),
        // then the surfaces are assumed to be facing different directions.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int i; // DO loop counters for surfaces in the zone
        int j;
        Array1D<Real64> ZoneArea; // Sum of the area of all zone surfaces seen

        // FLOW:
        // Calculate the sum of the areas seen by all zone surfaces
        // First Zone.Area is a vector of 0s with length equal to N
        ZoneArea.dimension(N, 0.0);
        for (i = 1; i <= N; ++i) {
            for (j = 1; j <= N; ++j) {
                // Assumption is that a surface cannot see itself or any other surface
                // that is facing the same direction (has the same azimuth)
                //  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
                //  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
                //  Skip same surface
                if (i == j) continue;
                //  Include INTMASS, FLOOR(for others), CEILING, ROOF  and different facing surfaces.
                //  Roofs/ceilings always see floors
                if ((Surface(SPtr(j)).Class == SurfaceClass_IntMass) || (Surface(SPtr(j)).Class == SurfaceClass_Floor) ||
                    (Surface(SPtr(j)).Class == SurfaceClass_Roof && Surface(SPtr(i)).Class == SurfaceClass_Floor) ||
                    ((std::abs(Azimuth(i) - Azimuth(j)) > SameAngleLimit) ||
                     (std::abs(Tilt(i) - Tilt(j)) >
                      SameAngleLimit))) { // Everything sees internal mass surfaces | Everything except other floors sees floors
                    // Second: sum the seen areas (angles lower than 10 are not considered)
                    // ZoneArea(i) is the sum of areas seen by the surface 'i'
                    ZoneArea(i) += A(j);
                }
            }
            if (ZoneArea(i) <= 0.0) {
                ShowWarningError("CalcApproximateViewFactors: Zero area for all other zone surfaces.");
                ShowContinueError("Happens for Surface=\"" + Surface(SPtr(i)).Name + "\" in Zone=" + Zone(Surface(SPtr(i)).Zone).Name);
            }
        }

        // Set up the approximate view factors.  First these are initialized to all zero.
        // This will clear out any junk leftover from whenever.  Then, for each zone
        // surface, set the view factor from that surface to other surfaces as the
        // area of the other surface divided by the sum of the area of all zone surfaces
        // that the original surface can actually see (calculated above).  This will
        // allow that the sum of all view factors from the original surface to all other
        // surfaces will equal unity.  F(I,J)=0 if I=J or if the surfaces face the same
        // direction.
        //  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
        //  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
        // The second IF statement is intended to avoid a divide by zero if
        // there are no other surfaces in the zone that can be seen.
        F = 0.0;
        for (i = 1; i <= N; ++i) {
            for (j = 1; j <= N; ++j) {

                //  Skip same surface

                if (i == j) continue;
                //  Include INTMASS, FLOOR(for others), CEILING/ROOF  and different facing surfaces.
                if ((Surface(SPtr(j)).Class == SurfaceClass_IntMass) || (Surface(SPtr(j)).Class == SurfaceClass_Floor) ||
                    (Surface(SPtr(j)).Class == SurfaceClass_Roof) ||
                    ((std::abs(Azimuth(i) - Azimuth(j)) > SameAngleLimit) || (std::abs(Tilt(i) - Tilt(j)) > SameAngleLimit))) {
                    // 
                    if (ZoneArea(i) > 0.0) F(j, i) = A(j) / (ZoneArea(i));
                }
            }
        }

        ZoneArea.deallocate();
    }


// SECOND FUNCTION

    void FixViewFactors(int const N,                     // NUMBER OF SURFACES
                        const Array1D<Real64> &A,        // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
                        Array2A<Real64> F,               // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
                        std::string &enclName,           // Name of Enclosure being fixed
                        std::vector<int> const zoneNums, // Zones which are part of this enclosure
                        Real64 &OriginalCheckValue,      // check of SUM(F) - N
                        Real64 &FixedCheckValue,         // check after fixed of SUM(F) - N
                        Real64 &FinalCheckValue,         // the one to go with
                        int &NumIterations,              // number of iterations to fixed
                        Real64 &RowSum                   // RowSum of Fixed
    )
    {

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine fixes approximate view factors and enforces reciprocity
        // and completeness.

        // METHODOLOGY EMPLOYED:
        // A(i)*F(i,j)=A(j)*F(j,i); F(i,i)=0.; SUM(F(i,j)=1.0, j=1,N)
        // Subroutine takes approximate view factors and enforces reciprocity by
        // averaging AiFij and AjFji.  Then it determines a set of row coefficients
        // which can be multiplied by each AF product to force the sum of AiFij for
        // each row to equal Ai, and applies them. Completeness is checked, and if
        // not satisfied, the AF averaging and row modifications are repeated until
        // completeness is within a preselected small deviation from 1.0
        // The routine also checks the number of surfaces and if N<=3, just enforces reciprocity.

        // Using/Aliasing
        using General::RoundSigDigits;
        
        // EP_SIZE_CHECK(array, min_size) assert(array.size() >= min_size)
        // if the condition is not satisfied, the warning message is printed:
        // Assertion failed: expression, file filename, line line number 

        // Argument array dimensioning
        EP_SIZE_CHECK(A, N);
        F.dim(N, N);

        // Locals
        // SUBROUTINE ARGUMENTS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const PrimaryConvergence(0.001);
        Real64 const DifferenceConvergence(0.00001);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 LargestArea;
        Real64 ConvrgNew;
        Real64 ConvrgOld;
        Real64 Accelerator;            // RowCoefficient multipler to accelerate convergence
        Real64 CheckConvergeTolerance; // check value for actual warning

        bool Converged;
        int i;
        int j;
        static int LargestSurf(0);

        // FLOW:
        OriginalCheckValue = std::abs(sum(F) - N);

        //  Allocate and zero arrays
        Array2D<Real64> FixedAF(F); // store for largest area check

        Accelerator = 1.0;
        ConvrgOld = 10.0;
        LargestArea = maxval(A);

        //  Check for Strange Geometry
        if (LargestArea > (sum(A) - LargestArea)) {
            for (i = 1; i <= N; ++i) {
                if (LargestArea != A(i)) continue;
                LargestSurf = i;
                break;
            }
            FixedAF(LargestSurf, LargestSurf) = min(0.9, 1.2 * LargestArea / sum(A)); // Give self view to big surface
        }

        //  Set up AF matrix.
        Array2D<Real64> AF(N, N); // = (AREA * DIRECT VIEW FACTOR) MATRIX
        for (i = 1; i <= N; ++i) {
            for (j = 1; j <= N; ++j) {
                AF(j, i) = FixedAF(j, i) * A(i);
            }
        }

        //  Enforce reciprocity by averaging AiFij and AjFji
        FixedAF = 0.5 * (AF + transpose(AF)); // Performance Slow way to average with transpose (heap use)

        AF.deallocate();

        Array2D<Real64> FixedF(N, N); // CORRECTED MATRIX OF VIEW FACTORS (N X N)

        NumIterations = 0;
        RowSum = 0.0;
        //  Check for physically unreasonable enclosures.

        if (N <= 3) {
            for (i = 1; i <= N; ++i) {
                for (j = 1; j <= N; ++j) {
                    FixedF(j, i) = FixedAF(j, i) / A(i);
                }
            }

            ShowWarningError("Surfaces in Zone/Enclosure=\"" + enclName + "\" do not define an enclosure.");
            ShowContinueError("Number of surfaces <= 3, view factors are set to force reciprocity but may not fulfill completeness.");
            ShowContinueError("Reciprocity means that radiant exchange between two surfaces will match and not lead to an energy loss.");
            ShowContinueError("Completeness means that all of the view factors between a surface and the other surfaces in a zone add up to unity.");
            ShowContinueError("So, when there are three or less surfaces in a zone, EnergyPlus will make sure there are no losses of energy but");
            ShowContinueError(
                "it will not exchange the full amount of radiation with the rest of the zone as it would if there was a completed enclosure.");

            RowSum = sum(FixedF);
            if (RowSum > (N + 0.01)) {
                // Reciprocity enforced but there is more radiation than possible somewhere since the sum of one of the rows
                // is now greater than unity.  This should not be allowed as it can cause issues with the heat balance.
                // Correct this by finding the largest row summation and dividing all of the elements in the F matrix by
                // this max summation.  This will provide a cap on radiation so that no row has a sum greater than unity
                // and will still maintain reciprocity.
                Array1D<Real64> sumFixedF;
                Real64 MaxFixedFRowSum;
                sumFixedF.allocate(N);
                sumFixedF = 0.0;
                for (i = 1; i <= N; ++i) {
                    for (j = 1; j <= N; ++j) {
                        sumFixedF(i) += FixedF(i, j);
                    }
                    if (i == 1) {
                        MaxFixedFRowSum = sumFixedF(i);
                    } else {
                        if (sumFixedF(i) > MaxFixedFRowSum) MaxFixedFRowSum = sumFixedF(i);
                    }
                }
                sumFixedF.deallocate();
                if (MaxFixedFRowSum < 1.0) {
                    ShowFatalError(" FixViewFactors: Three surface or less zone failing ViewFactorFix correction which should never happen.");
                } else {
                    FixedF *= (1.0 / MaxFixedFRowSum);
                }
                RowSum = sum(FixedF); // needs to be recalculated
            }
            FinalCheckValue = FixedCheckValue = std::abs(RowSum - N);
            F = FixedF;
            for (int zoneNum : zoneNums) {
                Zone(zoneNum).EnforcedReciprocity = true;
            }
            return; // Do not iterate, stop with reciprocity satisfied.

        } //  N <= 3 Case

        //  Regular fix cases
        Array1D<Real64> RowCoefficient(N);
        Converged = false;
        while (!Converged) {
            ++NumIterations;
            for (i = 1; i <= N; ++i) {
                // Determine row coefficients which will enforce closure.
                Real64 const sum_FixedAF_i(sum(FixedAF(_, i)));
                if (std::abs(sum_FixedAF_i) > 1.0e-10) {
                    RowCoefficient(i) = A(i) / sum_FixedAF_i;
                } else {
                    RowCoefficient(i) = 1.0;
                }
                FixedAF(_, i) *= RowCoefficient(i);
            }

            //  Enforce reciprocity by averaging AiFij and AjFji
            FixedAF = 0.5 * (FixedAF + transpose(FixedAF));

            //  Form FixedF matrix
            for (i = 1; i <= N; ++i) {
                for (j = 1; j <= N; ++j) {
                    FixedF(j, i) = FixedAF(j, i) / A(i);
                    if (std::abs(FixedF(j, i)) < 1.e-10) {
                        FixedF(j, i) = 0.0;
                        FixedAF(j, i) = 0.0;
                    }
                }
            }

            ConvrgNew = std::abs(sum(FixedF) - N);
            if (std::abs(ConvrgOld - ConvrgNew) < DifferenceConvergence || ConvrgNew <= PrimaryConvergence) { //  Change in sum of Fs must be small.
                Converged = true;
            }
            ConvrgOld = ConvrgNew;
            if (NumIterations > 400) { //  If everything goes bad,enforce reciprocity and go home.
                //  Enforce reciprocity by averaging AiFij and AjFji
                FixedAF = 0.5 * (FixedAF + transpose(FixedAF));

                //  Form FixedF matrix
                for (i = 1; i <= N; ++i) {
                    for (j = 1; j <= N; ++j) {
                        FixedF(j, i) = FixedAF(j, i) / A(i);
                    }
                }
                Real64 const sum_FixedF(sum(FixedF));
                FinalCheckValue = FixedCheckValue = CheckConvergeTolerance = std::abs(sum_FixedF - N);
                if (CheckConvergeTolerance > 0.005) {
                    ShowWarningError("FixViewFactors: View factors not complete. Check for bad surface descriptions or unenclosed zone=\"" +
                                     enclName + "\".");
                    ShowContinueError("Enforced reciprocity has tolerance (ideal is 0)=[" + RoundSigDigits(CheckConvergeTolerance, 6) +
                                      "], Row Sum (ideal is " + RoundSigDigits(N) + ")=[" + RoundSigDigits(RowSum, 2) + "].");
                    ShowContinueError("If zone is unusual, or tolerance is on the order of 0.001, view factors are probably OK.");
                }
                if (std::abs(FixedCheckValue) < std::abs(OriginalCheckValue)) {
                    F = FixedF;
                    FinalCheckValue = FixedCheckValue;
                }
                RowSum = sum_FixedF;
                return;
            }
        }
        FixedCheckValue = ConvrgNew;
        if (FixedCheckValue < OriginalCheckValue) {
            F = FixedF;
            FinalCheckValue = FixedCheckValue;
        } else {
            FinalCheckValue = OriginalCheckValue;
            RowSum = sum(FixedF);
            if (std::abs(RowSum - N) < PrimaryConvergence) {
                F = FixedF;
                FinalCheckValue = FixedCheckValue;
            } else {
                ShowWarningError("FixViewFactors: View factors not complete. Check for bad surface descriptions or unenclosed zone=\"" + enclName +
                                 "\".");
            }
        }
    }
