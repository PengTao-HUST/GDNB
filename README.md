# GDNB
This is a light package to implement the GDNB (giant-component-based dynamic network biomarker/marker) 
algorithm, which is a data-driven and model-free method and can be used to detect the tipping points of 
the phase transition in complex systems.

---

## Install
```bash
pip install gdnb
```

---

## Demonstrate the intuition of GDNB with 2D Ising model
### 1. import packages
```python
import gdnb
gdnb.set_default_style()
```

### 2. perform MC simulations under 9 temperatures (from 1 to 5 with an interval of 0.5)
```python
lattice = 10 # 10 x 10 2D Ising model
sim_steps = 100000 # simulation length
seed = 1 # random number seed

traj = gdnb.load_ising(lattice=lattice, sim_steps=sim_steps, seed=seed)
```
visualize the simulations
```python
import numpy as np
import matplotlib.pyplot as plt

def plot_traj(traj, c='b'):
    '''plot the simulated trajectory.'''
    avg_mag = np.mean(traj, axis=1)
    ts = np.arange(1, 5.1, .5)
    fig, ax = plt.subplots(len(traj), 1, figsize=(6, 10),
                           sharex=True, sharey=True)
    for i, mag in enumerate(avg_mag):
        ax[i].plot(mag, c=c)
        ax[i].set_ylim(-1.1, 1.1)
        ax[i].set_yticks(np.arange(-1, 1.1, .5))
        ax[i].set_ylabel('T={}'.format(ts[i]), fontsize=15)
        ax[i].tick_params(direction='in', labelsize=13)
    ax[-1].set_xlabel('Snapshot', fontsize=15)
    fig.tight_layout()    
    return fig, ax

fig, ax = plot_traj(traj)
``` 
This figure shows that the average magnetizations, defined by the average absolute value of all spins, 
change over time at different temperatures.

![traj](https://github.com/PengTao-HUST/GDNB/blob/master/figures/traj.png?raw=true)

```python
def cal_mean_std(traj):
    '''calculate the average magnetization and corresponding SD.'''
    means = []
    stds = []
    for i, state in enumerate(traj):    
        tmp = np.sum(state, axis=0) / len(state)
        means.append(abs(np.mean(tmp)))
        stds.append(np.std(tmp))
    return means, stds

def plot_magnetization(means, stds, c='teal'):
    '''plot the average magnetization with errorbar.'''       
    fig, ax = plt.subplots(figsize=(6, 4))
    ax.errorbar(range(len(means)), means, yerr=stds, fmt='-o', capsize=5,
                capthick=2, c=c)
    ax.tick_params(direction='in', labelsize=13)
    ax.grid(axis='y')
    ax.set_xticks(np.arange(9))
    ax.set_xticklabels(np.arange(1., 5.1, .5))
    ax.set_yticks(np.arange(-.6, 1.21, .3))
    ax.set_xlabel('Temperature', fontsize=15)
    ax.set_ylabel('Magnetization', fontsize=15)
    fig.tight_layout()
    return fig, ax

means, stds = cal_mean_std(traj)
fig, ax = plot_magnetization(means, stds)
```
![mag](https://github.com/PengTao-HUST/GDNB/blob/master/figures/mag.png?raw=true)

It can be seen that our simulations can properly describe the phase transition of the Ising model.

### 3.apply the GDNB algorithm to this simulated data
#### 3.1 instantiation
The core class of this package is `GDNBData`. Using a 3D array (`m`x`n`x`s`) to instantiate, 
where `m`, `n` and `s` are the numbers of observations, variables and repeats, respectively.
```python
mydata = gdnb.GDNBData(traj)
```

#### 3.2 Use `.fit()` to perform analysis
```python
pv_cut = 0.05
pcc_cut = 0.6
mydata.fit(pv_cut=pv_cut, pcc_cut=pcc_cut)
```

#### 3.3 visualize the results of GDNB
```python
fig, ax = mydata.plot_index(xticklabels=np.arange(1, 5.1, 0.5), xlabel='Temperature')
```
![index](https://github.com/PengTao-HUST/GDNB/blob/master/figures/index.png?raw=true)

The composite index (CI) has an obvious peak at T=2.5, indicating the transition temperature 
of this system is 2.5, which agrees well with the theoretical critical temperature (2.27) 
among all 9 temperatures. To show the transition more clear, the transition core is mapped 
on the Ising model.

```python
def plot_ising_maps(data, lattice=10, s=100, ncol=3, figsize=(12, 12), 
                    c_list=['wheat','gray','red']):
    points = np.array([
        [j, i] for i in range(lattice) for j in range(lattice)])
    m = len(data.sele_index)
    Ts = np.arange(1, 5.1, .5)
    fig, ax = plt.subplots(int(m/ncol), ncol, figsize=figsize)
    ax=ax.flatten()

    for t in range(m):
        colors=[c_list[0]] * lattice ** 2
        for i in range(lattice ** 2):
            if i in data.sele_index[t][
                    np.where(
                        data.cls_index[t] == data.largest_cls_index[t])]:
                colors[i] = c_list[2]
            elif i in data.sele_index[t]:
                colors[i] = c_list[1]
            else:
                continue
        ax[t].scatter(points[:,0], points[:,1], color=colors, s=s)
        ax[t].set_title('T={:2.1f}'.format(Ts[t]), fontsize=18)
        ax[t].set_axis_off()        
    return fig, ax

fig, ax = plot_ising_maps(mydata)
```
![map](https://github.com/PengTao-HUST/GDNB/blob/master/figures/map.png?raw=true)
As shown in this figure, The giant component (red dots) only appears at T=2.5.

---

### Reproduce the results in the paper
To reproduce the results in the paper, check the notebook files and results 
in [paper_examples](https://github.com/PengTao-HUST/GDNB/tree/master/paper_examples).

---

### License
MIT License