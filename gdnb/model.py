# -*- coding: utf-8 -*-
from tqdm import tqdm

import matplotlib.pyplot as plt
import numpy as np
import scipy.stats as stats
from scipy.cluster.hierarchy import linkage, fcluster
import seaborn as sns


__all__ = ['GDNBData']


class GDNBData(object):
    """
    the core class for the GDNB method, the instance of this class should
    be a 3D numpy array.

    Attributes
    ----------
    m, n, s:
        number of windows, variables, samples.
    sele_index: list
        the index of the selected variables for each window.
    cls_index: list
        the index of the variables after cluster for each window.
    largest_cls: list
        the index of the variables in the giant component for each window.
    num_var_in_largest_cls: list
        number of variables in the giant component for each window.
    largest_cls_avg_pcc: list
        average absolute PCC of the giant component for each window.
    largest_cls_avg_std: list
        average relative fluctuation of the giant component for each window.
    composite_index: list
        composite index for each window.
    """

    def __init__(self, data):
        assert data.ndim == 3, 'the data should be a M x N x S array!'
        self.data = data
        self.m, self.n, self.s = self.data.shape

        print('Read the input data ...')
        print(f'The input data shape is: {self.m} x {self.n} x {self.s}')
        print(f'Number of windows: {self.m}')
        print(f'Number of variables: {self.n}')
        print(f'Number of samples: {self.s}\n')
        print('The next step is fitting data by .fit()')

    def fit(self,
            pv_cut=0.05,
            pcc_cut=0.9,
            pv_mode='row',
            std_top=None,
            cls_method='single',
            fdr=False,
            progress_bar=True,
            pbar_mode=None,
            print_summary=True):
        """
        the core function to implement the GDNB algorithm.

        Parameters
        ----------
        pv_cut: float
            cutoff of the p-value, which is used to select the variables with
            significant high fluctuation.
        pcc_cut: float
            cutoff of the absolute PCC value, which is used to cluster the
            selected variables. Note that `1-pcc_cut` is the metric for the cluster.
        pv_mode: 'row' or 'col'
            calculate the p-values by row or column.
        std_top: None or int
            if not None, then directly select the variables by STD instead of p-value.
        cls_method: str
            method for hierarchical cluster in scipy.
        fdr: bool
            whether use the FDR correction.
        progress_bar: bool
            whether show the progress bar.
        pbar_mode: tqdm-like or None
            interface for progress bar class (eg, tqdm).
        print_summary: bool
            whether summarize the GDNB results.
        """

        if pbar_mode is None:
            pbar_mode = tqdm

        print('fitting data using GDNB method ...')

        print('step 1: select the variables with high fluctuation ...')
        self.std_data = np.std(self.data, axis=-1)
        pbar = pbar_mode(range(self.m)) if progress_bar else range(self.m)
        if std_top is None:
            if pv_mode == 'row':
                self.pv_mat = self._cal_pv_mat_by_row(fdr=fdr)
            else:
                self.pv_mat = self._cal_pv_mat_by_col(fdr=fdr)

            self.std_index = np.where(self.pv_mat < pv_cut)
            self.sele_index = [self.std_index[1][self.std_index[0] == i] for i in pbar]
        else:
            assert isinstance(std_top, int) and std_top > 1
            self.sele_index = [np.argsort(self.std_data[i])[::-1][:std_top] for i in pbar]

        self.sele_data = [self.data[i, self.sele_index[i]] for i in range(self.m)]

        print('step 2: cluster selected variables ...')
        pbar = pbar_mode(range(self.m)) if progress_bar else range(self.m)
        self.cls_index = [
            fcluster(
                linkage(self.sele_data[i], method=cls_method,
                        metric=lambda u, v: 1 - abs(stats.pearsonr(u, v)[0])),
                t=pcc_cut, criterion='distance')
            for i in pbar
        ]

        print('step 3: find the giant component (largest cluster) ...')
        pbar = pbar_mode(range(self.m)) if progress_bar else range(self.m)
        self.largest_cls_index = []
        for i in pbar:
            cls_sorted_index = [
                [c, len(self.cls_index[i][self.cls_index[i] == c])]
                for c in set(self.cls_index[i])
            ]
            cls_sorted_index.sort(key=lambda x: x[1])
            self.largest_cls_index.append(cls_sorted_index[-1][0])

        self.largest_cls = [
            self.sele_index[i][
                np.where(self.cls_index[i] == self.largest_cls_index[i])
            ]
            for i in range(self.m)
        ]

        print('fitting Done\n')

        self.num_var_in_largest_cls = self._num_var_in_largest_cls()
        self.largest_cls_avg_pcc = self._largest_cls_avg_pcc()
        self.largest_cls_avg_std = self._largest_cls_avg_std()
        self.composite_index = self.cal_composite_index()

        if print_summary:
            self.results_summary()

    def _cal_pv_mat_by_row(self, fdr=False):
        """
        calculate the p-value matrix by row.
        """
        if fdr:
            pv_mat = np.asarray([
                self.fdr_adjust(self.ttest_less(self.std_data[i]))
                for i in range(self.m)])
        else:
            pv_mat = np.asarray(([self.ttest_less(self.std_data[i])
                                  for i in range(self.m)]))
        return pv_mat

    def _cal_pv_mat_by_col(self, fdr=False):
        """
        calculate the p-value matrix by column.
        """
        if fdr:
            pv_mat = np.asarray([
                self.fdr_adjust(self.ttest_less(self.std_data[:, i]))
                for i in range(self.n)
            ])
        else:
            pv_mat = np.asarray(([self.ttest_less(self.std_data[:, i])
                                  for i in range(self.n)]))
        return pv_mat

    def _num_var_in_largest_cls(self):
        """
        calculate the number of the variables in each giant component.
        """
        num_var_in_largest_cls = [len(c) for c in self.largest_cls]
        return num_var_in_largest_cls

    def _largest_cls_avg_pcc(self):
        """
        calculate the average PCC for the largest class or giant component.
        """
        largest_cls_avg_pcc = []
        for i in range(self.m):
            n = len(self.largest_cls[i])
            if n > 1:
                cor_mat = np.corrcoef(self.data[i][self.largest_cls[i]])
                largest_cls_avg_pcc.append(
                    np.sum(np.abs(np.triu(cor_mat, 1))) * 2 / ((n - 1) * n))
            else:
                largest_cls_avg_pcc.append(0)
        return largest_cls_avg_pcc

    def _largest_cls_avg_std(self):
        """
        calculate the average STD for the largest class or giant component.
        """
        largest_cls_avg_std = []
        for i in range(self.m):
            n = len(self.largest_cls[i])
            if n > 1:
                largest_cls_avg_std.append(
                    self.std_data[i][self.largest_cls[i]].mean())
            else:
                largest_cls_avg_std.append(0)
        return largest_cls_avg_std

    def cal_composite_index(self):
        """
        calculate the composite index.
        """
        composite_index = [
            n * p * s for n, p, s in zip(
                self.num_var_in_largest_cls,
                self.largest_cls_avg_pcc,
                self.largest_cls_avg_std
            )
        ]
        return composite_index

    def results_summary(self):
        """
        print the main results of GDNB.
        """
        print('-' * 15 + ' Results Summary ' + '-' * 15)
        print('number of variables in each largest cluster for each window:')
        print(self.num_var_in_largest_cls)
        print('\naverage PCC of each largest cluster for each window:')
        print(self.largest_cls_avg_pcc)
        print('\naverage STD of each largest cluster for each window:')
        print(self.largest_cls_avg_std)
        print('\ncomposite index for each window:')
        print(self.composite_index)

    def plot_clusters(self, method='single', c_list=['gray', 'red'], cmap='seismic',
                      sort=True, figsize=(10, 6), savefig=False, fig_path='./',
                      dpi=200, fig_fmt='png', n_split=2, zscore=True, ticklabels=False,
                      max_scale=1, **kwargs):
        """
        generate hierarchical cluster plots for each window.

        Parameters
        ----------
        method: str
            method used to cluster the data
        c_list: list
            color list for annotation of giant component or not
        cmap: str
            cmap for the heatmap
        sort: bool
            reverse the values for specific variables
        figsize: tuple
            figure size
        savefig: book
            whether save the figures
        fig_path: str
            path to save the figures
        dpi: int
            the resolution for the figure
        fig_fmt: str
            format of figures, eg, png, svg, jpg ..., passed to `plt.savefig`
        n_split: int
            divide the data into n_split component when sort is Ture
        zscore: bool
            whether normalize the data
        ticklabels:
            ticklabels for x-axis
        max_scale:
            scale parameter to the max absolute value
        kwargs:
            other parameters will be send to sns.clustermap

        Returns
        -------
        gs: list
            a list of the figures
        """

        gs = []
        for i in range(self.m):
            colors = np.array([c_list[0]] * len(self.cls_index[i]))
            colors[
                np.where(self.cls_index[i] == self.largest_cls_index[i])
            ] = c_list[1]

            if zscore:
                if sort:
                    tmpdat = np.apply_along_axis(
                        self.reverse, 1, np.apply_along_axis(
                            stats.zscore, 1, self.sele_data[i]),
                        n_split=n_split)
                else:
                    tmpdat = np.apply_along_axis(
                        stats.zscore, 1, self.sele_data[i])
            else:
                if sort:
                    tmpdat = np.apply_along_axis(
                        self.reverse, 1, self.sele_data[i],
                        n_split=n_split)
                else:
                    tmpdat = self.sele_data[i]

            absmax = np.max(np.abs(tmpdat))
            absmax *= max_scale
            tlabels = 'auto' if ticklabels else False
            g = sns.clustermap(
                tmpdat.T, method=method, vmin=-absmax, vmax=absmax,
                metric=lambda u, v: 1 - abs(stats.pearsonr(u, v)[0]),
                cmap=cmap, row_cluster=False, figsize=figsize,
                col_colors=colors, xticklabels=tlabels, yticklabels=tlabels,
                **kwargs)

            gs.append(g)
            if savefig:
                g.savefig(fig_path + f'cluster{i + 1}.{fig_fmt}', dpi=dpi, format=fig_fmt)
                plt.close()

        return gs

    def plot_index(self, figsize=(8, 8), scale=0.2, xlabel=None, ylabels=None,
                   xticks=None, xticklabels=None, colors=None, grid_x=False,
                   hspace=0.4, **kwargs):
        """
        plot GC, RF, |PCC| and the composite index.

        Parameters
        ----------
        figsize: tuple
            figure size
        scale: float
            the ratio of the blank region of y-axis
        xlabel: None or str
            label for the x-axis
        ylabels: list of str
            y labels for 4 subplots
        xticks: list of float
            ticks for the x-axis
        xticklabels: list of str
            ticklabels for the x-axis
        colors:
            colors for 4 lines
        grid_x: bool
            whether show grid of x-axis
        hspace: float
            hspace for `plt.subplot_adjust()`
        kwargs:
            other parameters will be passed to `ax.plot()`

        Returns
        -------
        fig, ax
        """

        plot_data = [
            self.num_var_in_largest_cls,
            self.largest_cls_avg_std,
            self.largest_cls_avg_pcc,
            self.composite_index]

        if ylabels is None:
            ylabels = ['GC', 'RF', '|PCC|', 'CI']
        cs = ['b', 'b', 'b', 'r'] if colors is None else colors

        fig, ax = plt.subplots(4, 1, figsize=figsize)
        for i in range(4):
            ax[i].plot(plot_data[i], cs[i], **kwargs)
            ax[i].set_ylabel(ylabels[i])
            if xticks is not None:
                ax[i].set_xticks(xticks)
            else:
                ax[i].set_xticks(range(self.m))
            if xticklabels is not None:
                ax[i].set_xticklabels(xticklabels)
            if grid_x:
                ax[i].grid(True, axis='x')
            self.set_ylim_scale(plot_data[i], ax[i], scale=scale)
        ax[-1].set_xlabel(xlabel)
        fig.subplots_adjust(hspace=hspace)

        return fig, ax

    # @staticmethod
    # def pcc_metric(u, v):
    #     return 1 - abs(stats.pearsonr(u, v)[0])

    @staticmethod
    def set_ylim_scale(y, ax, scale=0.2):
        """ set the blank space of y axis."""
        ymin, ymax = np.min(y), np.max(y)
        interval = ymax - ymin
        ax.set_ylim(ymin - scale * interval,
                    ymax + scale * interval)

    @staticmethod
    def reverse(row, n_split=2):
        """ reverse the sign of the input row."""
        if np.sum(row[:len(row) // n_split]) < 0:
            return -row
        else:
            return row

    @staticmethod
    def ttest_less(inp):
        """ calculated the ond-side Student's t-test."""
        s = np.std(inp, ddof=1)
        m = np.mean(inp)
        n = len(inp)
        p_values = np.zeros(n)
        for i, v in enumerate(inp):
            j = (m - v) / s * np.sqrt(n)
            p_values[i] = stats.t.cdf(j, n - 1)
        return p_values

    @staticmethod
    def fdr_adjust(p_values):
        """ adjust the p-values using FDR correction."""
        l = len(p_values)
        pv_sorted = np.sort(p_values)
        index = np.argsort(p_values)
        tmp = np.asarray(
            [v * (l / (i + 1)) for i, v in enumerate(pv_sorted)])
        out = np.zeros(l)
        for i, v in enumerate(index):
            out[v] = tmp[i]
        return out
