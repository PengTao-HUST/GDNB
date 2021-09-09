# -*- coding: utf-8 -*-

import numpy as np
from tqdm import tqdm


__all__ = ['Ising2DSimulator']


class Ising2DSimulator(object):
    """
    perform MC simulations for 2D Ising models.
    """
    def __init__(self,
                 lattice=10,
                 relax_step=100000,
                 use_step=100000,
                 save_interval=100,
                 T_windows=np.arange(1, 5.1, 0.5),
                 J=1,
                 H=0,
                 K=1):
        """
        Parameters
        ----------
        lattice: int
            the shape of 2D Ising model is lattice x lattice.
        relax_step: int
            the number of steps before equilibrium, corresponding trajectory will not be saved.
        use_step: int
            the number of steps will be used
        save_interval: int
            save states per save_interval steps
        T_windows: list-like
            the temperature list used for simulation
        J: float
            coupling parameter for two spins
        H: float
            external magnetic field
        K: float
            Boltzmann constant
        """

        self.lattice = lattice
        self.relax = relax_step
        self.use = use_step
        self.savestep = save_interval
        self.Twindows = T_windows
        self.J = J
        self.H = H
        self.K = K
    
    def rand_init_state(self):
        """
        generate random initial state.
        """
        state = np.random.random((self.lattice, self.lattice))
        state[state >= 0.5] = 1
        state[state < 0.5] = -1
        return state

    def rig_init_state(self, value=1):
        """
        generate rigid initial state.
        """
        if value == 1:
            state = np.ones((self.lattice, self.lattice))
        elif value == -1:
            state = -np.ones((self.lattice, self.lattice))
        else:
            raise ValueError("spin state must be 1 or -1.")
        return state

    def cal_delta_E(self, state, i, j):
        """
        calculate energy difference after flip at position (i, j).
        """
        top = i - 1
        bottom = (i + 1) % self.lattice
        left = j - 1
        right = (i + 1) % self.lattice
        delta_E = 2 * state[i, j] * (self.J * (
            state[top, j] + state[bottom, j] + 
            state[i, left] + state[j, right]) + self.H)
        return delta_E

    def MC_single_T(self, state, T):
        """
        perform MC simulation at single temperature.
        """
        states = []
        for s in range(self.use + self.relax):
            i = np.random.randint(0, self.lattice)
            j = np.random.randint(0, self.lattice)
            delta_E = self.cal_delta_E(state, i, j)
            if delta_E <= 0:
                state[i, j] = -state[i, j]
            else:
                if np.exp((-delta_E) / (self.K * T)) > np.random.random():
                    state[i, j] = -state[i, j]

            if s >= self.relax and s % self.savestep == 0:
                states.append(list(state.flatten()))
        return np.asarray(states).T

    def MC_mul_T(self, init='random', value=1, progress_bar=True, pbar_mode=None):
        """
        perform MC simulations at multiple temperatures.
        """
        if pbar_mode is None:
            pbar_mode = tqdm

        if progress_bar:
            pbar = pbar_mode(self.Twindows)
        else:
            pbar = self.Twindows

        if init == 'random':
            state = self.rand_init_state()
        else:
            state = self.rig_init_state(value=value)
            
        mul_states = []
        for T in pbar:
            states = self.MC_single_T(state, T)
            mul_states.append(states)
        return np.array(mul_states)

    def run(self, init='random', value=1, progress_bar=True, pbar_mode=None):
        """ main interface for simulations.

        Parameters
        ----------
        init:
            how to initialize the Ising model, default is 'random', otherwise,
            all spins will be set to 'value'.
        value: 1 or -1
            1 is up, -1 is down
        progress_bar: Bool
            whether show progress bar
        pbar_mode: tqdm-like or None
            interface for progress bar class (eg, tqdm).

        Returns
        -------
            simulated trajectories.
        """
        self.traj = self.MC_mul_T(init=init, value=value, progress_bar=progress_bar,
                                  pbar_mode=pbar_mode)
        return self.traj

    def save_traj(self, name=None, dtype='int8'):
        """
        save simulated trajectories.
        """
        if name is None:
            name = 'traj.npy'
        np.save(name, self.traj.astype(dtype))
