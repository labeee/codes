import json
import numpy as np
import pandas as pd
from SALib.sample import saltelli

# define range
def DefRange(bound, gap):
  mult = (bound[1] - bound[0])*gap
  inter = [bound[0] - mult, bound[1] + mult]
  return(inter)

# generate sample
def GenSample(names, bounds, gap, qual_vars, size, output_dir, save_problem = True):
    consts = np.repeat(gap, len(names))
    consts[qual_vars] = 0
    bounds = [DefRange(bound, const) for bound, const in zip(bounds, consts)]
    problem = {'num_vars': len(names), 'names': names, 'bounds': bounds}
    param_values = saltelli.sample(problem, size)
    df = pd.DataFrame(param_values, columns = problem['names'])
    df.to_csv(output_dir + 'saltelli_sample_multi.csv', index = False)
    if save_problem:
      problem_path = output_dir + 'sobol_problem_multi.json'
      with open(problem_path, 'w') as file:
        json.dump(problem, file, indent = 4)

# main code
names = ['seed', 'storey', 'area', 'ratio', 'height', 'azimuth', 'shell_wall',
         'abs_wall', 'shell_roof', 'abs_roof', 'wwr_liv', 'wwr_dorm', 'u_window',
         'shgc', 'open_factor', 'blind', 'balcony', 'facade', 'mirror', 'dbt']
bounds = [[1, 5], [1, 4], [50, 150], [0.5, 2], [2.5, 3.5], [0, 360], [1, 7],
          [0.2, 0.8], [1, 5], [0.2, 0.8], [0.2, 0.8], [0.2, 0.8], [2.8, 5.7],
          [0.22, 0.87], [0.4, 0.9], [0, 2], [-1.5, 2.5], [1, 3], [0, 2], [10.83, 28.24]]
qual_vars = [0, 1, 6, 8, 15, 17, 18]
output_dir = '/home/rodox/git/labeee/mlm/'
GenSample(names, bounds, 0.02, qual_vars, 4000, output_dir)

