import pandas as pd
import numpy as np
import pingouin as pg
from scipy import stats
from sklearn import metrics

from statsmodels.stats.outliers_influence import variance_inflation_factor

df = pd.read_csv('data/proc/regression.csv')
meta = pd.read_csv('data/proc/regression_meta.csv')
meta.loc[meta['type'] == 'ordinal', 'type'] = 'numeric' # doesnt matter here

# test normality -------------------------------------------------------------------------------------------------------
vars_num = [
    'red_per',
    'red_residential', 'red_transport', 'red_industry', 'red_municipal', 'red_other',
    'co2_target', 'bei_ppc',
    'y_bei', 'y_start', 'y_submission', 'y_mei', 'y_plan', 'y_diff', 'n_mon'
]

for var in vars_num:
    print(var, stats.shapiro(x=df[var]))


# calculate correlation matrix -----------------------------------------------------------------------------------------
index = pd.MultiIndex.from_product(
    [meta['colname'], meta['colname']],
    names=['var1', 'var2']
)
out = pd.DataFrame(index=index).reset_index()
out = out.loc[(out['var1'] != 'pop_group') & (out['var2'] != 'pop_group')]
out = out.merge(meta.rename(columns={'colname': 'var1', 'type': 'type1'}), how='left', on='var1')
out = out.merge(meta.rename(columns={'colname': 'var2', 'type': 'type2'}), how='left', on='var2')
out['test'] = out['type1'] + '_' + out['type2']
out['cor'] = np.nan
out['pvalue'] = np.nan

results = pd.DataFrame()
for size in ['small', 'large']:
    df_s = df.loc[df['pop_group'] == size]
    for idx, row in out.iterrows():
        print(idx)
        aux = df_s[[row['var1'], row['var2']]].dropna()
        x = aux.iloc[:, 0]
        y = aux.iloc[:, 1]
        if row['var1'] == row['var2']:
            cor = 1
            pvalue = np.nan
        elif row['test'] == 'numeric_numeric':
            cor = stats.spearmanr(a=x, b=y).correlation
            pvalue = stats.spearmanr(a=x, b=y).pvalue
        elif row['test'] == 'binary_binary':
            mat = np.transpose(np.vstack([x, y]))
            cor = metrics.matthews_corrcoef(x, y)
            pvalue = np.nan
        elif row['test'] in ['numeric_binary', 'binary_numeric']:
            cor = stats.pointbiserialr(x=x, y=y).correlation
            pvalue = stats.pointbiserialr(x=x, y=y).pvalue
        out['cor'].iloc[idx] = cor
    out['pop_group'] = size
    results = results.append(out)
# save
results.to_csv('data/proc/correlation_matrix.csv', index=False)
