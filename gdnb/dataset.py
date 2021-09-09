# -*- coding: utf-8 -*-
import os
import re
import sys
from urllib.request import urlretrieve


import numpy as np


from .ising_sim import Ising2DSimulator
from .utils import normalize_by_mean


__all__ = ['load_ising', 'load_pathway', 'load_muscle']


def load_ising(lattice=10, seed=1, sim_steps=100000, init='random',
               value=1, progress_bar=True, pbar_mode=None, **kwargs):
    """
    load the simulated trajectories of 2D Ising model.

    Parameters
    ----------
    lattice: int
        the shape of 2D Ising model is lattice x lattice.
    seed: int
        random number seed
    sim_steps: int
        simulated steps
    init:
        how to initialize the Ising model, default is `random`, otherwise,
        all spins will be set to `value`.
    value: 1 or -1
        1 is up, -1 is down
    progress_bar:
        whether show the progress bar
    pbar_mode: tqdm-like or None
        interface for progress bar class (eg, tqdm).
    kwargs:
        other parameters will be send to `Ising2DSimulator`.

    Returns
    -------
        simulated trajectories.
    """
    np.random.seed(seed)
    sim = Ising2DSimulator(lattice=lattice, relax_step=sim_steps, use_step=sim_steps, **kwargs)
    traj = sim.run(init=init, value=value, progress_bar=progress_bar, pbar_mode=pbar_mode)
    return traj


def load_pathway(path_idx=1, preprocess=True):
    """ Load the folding pathway example of the protein HP35.
    
    Parameters
    ----------
    path_idx: 1 or 2
        the index of the pathway, must be 1 or 2.
    preprocess: bool
        whether preprocess the expression data

    Returns
    -------
    if preprocess, return the distance data, otherwise, return the path of the
    origin data.
    """

    assert path_idx in [1, 2], 'Unavailable index, must be 1 or 2.'
    url = f'https://raw.githubusercontent.com/PengTao-HUST/GDNB/master/data/pathway{path_idx}.txt'
    cache_dir = sys.modules['gdnb'].__path__[0] + '/data/'

    if not os.path.exists(cache_dir):
        os.makedirs(cache_dir)

    data_file = os.path.basename(url)
    full_path = cache_dir + data_file

    if not os.path.exists(full_path):
        urlretrieve(url, cache_dir + data_file)

    if preprocess:
        traj = np.loadtxt(full_path)
        traj = np.apply_along_axis(normalize_by_mean, 0, traj[:, 1:])
        disdat = traj.reshape(10, 50, -1).transpose((0, 2, 1))
        return disdat
    else:
        return full_path


def load_muscle(preprocess=True, mean_cut=None):
    """ Load the gene expression data of the muscle of mouses.

    Parameters
    ----------
    preprocess: bool
        whether preprocess the expression data
    mean_cut: int or None
        if preprocess, genes with extremely low expression value (default: 2^7) will
        be discarded.

    Returns
    -------
    if preprocess, return the expression data, otherwise, return the path of the
    origin data.
    """

    url = f'https://raw.githubusercontent.com/PengTao-HUST/GDNB/master/data/muscle.txt'
    cache_dir = sys.modules['gdnb'].__path__[0] + '/data/'

    if not os.path.exists(cache_dir):
        os.makedirs(cache_dir)

    data_file = os.path.basename(url)
    full_path = cache_dir + data_file

    if not os.path.exists(full_path):
        urlretrieve(url, cache_dir + data_file)

    if preprocess:
        probes = []
        names = []
        exprs = []
        with open(full_path, 'r') as f:
            f.readline()
            f.readline()
            for line in f.readlines():
                data = line.split('\t')
                probe = data[0]
                name = data[1]
                expr = np.array([np.log2(float(x)) for x in data[2:]])
                probes.append(probe)
                names.append(name)
                exprs.append(expr)
        probes = np.asarray(probes)
        names = np.asarray(names)
        exprs = np.asarray(exprs)

        use_index = []
        for name in names:
            if re.match('.*///.*', name) or not re.match('^[A-Z].*', name):
                use_index.append(False)
            else:
                use_index.append(True)

        probes, names, exprs = [obj[use_index] for obj in [probes, names, exprs]]

        means = np.mean(exprs, axis=1)
        if mean_cut is None:
            mean_cut = 7

        cut_index = np.where(means > mean_cut)
        probes_c, names_c, exprs_c = [obj[cut_index] for obj in [probes, names, exprs]]
        exprs_c = np.apply_along_axis(normalize_by_mean, 1, exprs_c)

        exprs_split = np.array([exprs_c[:, 2 * i: 2 * i + 6] for i in range(25)])
        return probes_c, names_c, exprs_split
    else:
        return full_path
