import matplotlib.pyplot as plt
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
df.rename(columns={'Unnamed: 0': 'Name'}, inplace=True)
df.set_index('Name', inplace=True)
print(df.describe())
fig = plt.figure()
gs = fig.add_gridspec(10, 10)
for r in range(10):
    for c in range(10):
        axes = fig.add_subplot(gs[r, c])
        axes.xaxis.set_visible(False)
        axes.yaxis.set_visible(False)
        if r == c:
            axes.annotate(df.columns.values[r], (0.5, 0.5),
                xycoords='axes fraction', ha='center', va='center')
        else:
            df.plot.scatter(x=r, y=c, ax=axes)
plt.show()
fig = plt.figure(constrained_layout=True)
gs = fig.add_gridspec(1,2)
df[df['Private']=='Yes'].Outstate.plot.box(ax=fig.add_subplot(gs[0,0]))
df[df['Private']=='No'].Outstate.plot.box(ax=fig.add_subplot(gs[0,1]))
plt.show()
df = df.assign(Elite = df.Top10perc > 50)
print(df.Elite.describe())
fig = plt.figure(constrained_layout=True)
gs = fig.add_gridspec(1,2)
df[df['Elite']==True].Outstate.plot.box(ax=fig.add_subplot(gs[0,0]))
df[df['Elite']==False].Outstate.plot.box(ax=fig.add_subplot(gs[0,1]))
plt.show()
fig = plt.figure(constrained_layout=True)
gs = fig.add_gridspec(4,4)
df['Apps'].plot.hist(bins=25, ax=fig.add_subplot(gs[0,0])).set_ylabel('Apps')
df['Accept'].plot.hist(bins=25, ax=fig.add_subplot(gs[0,1])).set_ylabel('Accept')
df['Enroll'].plot.hist(bins=25, ax=fig.add_subplot(gs[0,2])).set_ylabel('Enroll')
df['Top10perc'].plot.hist(bins=25, ax=fig.add_subplot(gs[0,3])).set_ylabel('Top10perc')
df['Top25perc'].plot.hist(bins=25, ax=fig.add_subplot(gs[1,0])).set_ylabel('Top25perc')
df['F.Undergrad'].plot.hist(bins=25, ax=fig.add_subplot(gs[1,1])).set_ylabel('F.Undergrad')
df['P.Undergrad'].plot.hist(bins=25, ax=fig.add_subplot(gs[1,2])).set_ylabel('P.Undergrad')
df['Room.Board'].plot.hist(bins=25, ax=fig.add_subplot(gs[1,3])).set_ylabel('Room.Board')
df['Books'].plot.hist(bins=25, ax=fig.add_subplot(gs[2,0])).set_ylabel('Books')
df['Personal'].plot.hist(bins=25, ax=fig.add_subplot(gs[2,1])).set_ylabel('Personal')
df['PhD'].plot.hist(bins=25, ax=fig.add_subplot(gs[2,2])).set_ylabel('PhD')
df['Terminal'].plot.hist(bins=25, ax=fig.add_subplot(gs[2,3])).set_ylabel('Terminal')
df['S.F.Ratio'].plot.hist(bins=25, ax=fig.add_subplot(gs[3,0])).set_ylabel('S.F.Ratio')
df['perc.alumni'].plot.hist(bins=25, ax=fig.add_subplot(gs[3,1])).set_ylabel('perc.alumni')
df['Expend'].plot.hist(bins=25, ax=fig.add_subplot(gs[3,2])).set_ylabel('Expend')
df['Grad.Rate'].plot.hist(bins=25, ax=fig.add_subplot(gs[3,3])).set_ylabel('Grad.Rate')
plt.show()
