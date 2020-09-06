import pandas as pd

print('\nKNN\n---')
d = {'X1': [ 0,   2,   0,   0,  -1,   1 ],
     'X2': [ 3,   0,   1,   1,   0,   1 ],
     'X3': [ 0,   0,   3,   2,   1,   1 ],
     'Y':  ['R', 'R', 'R', 'G', 'G', 'R']}
df = pd.DataFrame(data = d)
df = df.assign(dist = (df.X1**2 + df.X2**2 + df.X3**2)**(0.5))
df = df.sort_values(by='dist')
print(df)
print('K=1 =>', df.head(1).Y.to_numpy()[0])
print('K=3 =>', df.head(3).groupby('Y').count().sort_values(by='dist', # arbitrary
    ascending=False).index.values[0])

print('\nCollege.csv\n-----------')
df = pd.read_csv('College.csv')
print(df)
