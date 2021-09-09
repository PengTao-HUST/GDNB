# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt


__all__ = ['set_default_style', 'normalize_by_mean', 'normalize_by_zscore']


def set_default_style(style=None):
    """ Set plot style for matplotlib. """

    my_default_plot_style = {
        'xtick.direction': 'in',
        'ytick.direction': 'in',
        'font.size': 15,
        'lines.linewidth': 2,
        'font.weight': 'medium',
        'axes.labelweight': 'medium',
        'axes.linewidth': 1.5,
        'grid.linewidth': 1.5,
        'savefig.dpi': 300
    }

    if style:
        try:
            for k, v in style.items():
                my_default_plot_style[k] = v
        except:
            print('style update fails, check the input style!')
    plt.rcParams.update(my_default_plot_style)


def normalize_by_mean(array):
    """
    Normalize the array the its mean, used to calculate the relative fluctuation.

    Parameters
    ----------
    array: numpy array

    Returns
    -------
    the array after normailzation
    """
    return array / np.mean(array)


def normalize_by_zscore(array):
    """
    Normalize the array by zscore

    Parameters
    ----------
    array: numpy array

    Returns
    -------
    the array after normailzation
    """
    return (array - np.mean(array)) / np.std(array)
