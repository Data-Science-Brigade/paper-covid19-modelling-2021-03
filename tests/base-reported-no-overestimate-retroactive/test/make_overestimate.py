import os
import re
import datetime
import pandas as pd
import numpy as np
import scipy as sp
import scipy.stats
from tqdm import tqdm

SNAPSHOT_FOLDER = "big_test_snapshots_diarios"

def daterange(start_date, end_date, step=1):
    for n in range(0,int((end_date - start_date).days),step):
        yield start_date + datetime.timedelta(days=n)

def get_cases_and_deaths(all_snapshots_df, dat, days_before):
    dat = pd.to_datetime(dat, format="%Y-%m-%d")
    cases = {}
    deaths = {}
    for dat_snapshot in all_snapshots_df["dat_snapshot"].unique():
        d = pd.to_datetime(dat_snapshot, format="%Y-%m-%d")
        df = all_snapshots_df[all_snapshots_df["dat_snapshot"]==dat_snapshot].set_index("data_ocorrencia")
        dat_data = dat-pd.Timedelta(days=days_before)
        if d >= dat:
            cases[dat_snapshot] = df[df.index == dat_data]["casos"]
            deaths[dat_snapshot] = df[df.index == dat_data]["obitos"]
    return cases, deaths

def get_merged_df_with_snapshot(dfs, days_before, snapshot_to_idx):
    merged_df = None
    for dat_snapshot in dfs:
        df = dfs[dat_snapshot].to_frame()
        df["snapshot"] = dat_snapshot
        df["day"] = snapshot_to_idx[dat_snapshot] - days_before
        if merged_df is None:
            merged_df = df
        else:
            merged_df = pd.concat([merged_df, df])
    return merged_df

def analysis(all_snapshots_df, dat, days_before, snapshot_to_idx, plot=True):
    cases, deaths = get_cases_and_deaths(all_snapshots_df, dat, days_before)
    
    md = get_merged_df_with_snapshot(deaths, days_before, snapshot_to_idx)
    mc = get_merged_df_with_snapshot(cases, days_before, snapshot_to_idx)
    
    if plot:
        sns.lineplot(data=md, x="data_ocorrencia", y="obitos", hue="dat_snapshot", style="dat_snapshot", markers=True, dashes=False)
        plt.show()
        sns.lineplot(data=cases, x="data_ocorrencia", y="casos", hue="dat_snapshot", style="dat_snapshot", markers=True, dashes=False)
        plt.show()

    # We aggregate by snapshot because this function used to deal with ranges instead of single dates
    return (
        md.groupby("snapshot").agg({"obitos": np.sum, "day": lambda x: sp.stats.mode(x)[0]}),
        mc.groupby("snapshot").agg({"casos": np.sum, "day": lambda x: sp.stats.mode(x)[0]})
    )


def derivative(values):
    return np.concatenate([[0.],values[1:]-values[:-1]])

def step_derivative_pct(values, eps=None):
    return np.concatenate([[np.nan],(values[1:]-values[:-1])/(values[:-1])])

def integral(initial, d_values, initial_idx = 0):
    d_values = np.nan_to_num(d_values, nan=0, posinf=0, neginf=0)
    values = [np.nan] if initial_idx != 0 else [initial]
    for i, d in enumerate(d_values[1:], start=1):
        
        values.append((initial) if i==initial_idx else (values[-1]*(1+d)))
    return np.array(values)

def integral_nonzero_start(d, x, initial=1):
    return integral(initial, d, initial_idx = np.min(np.nonzero(x)))


def plot_df(data, x, y, ylabel, pct=True):
    sns.lineplot(data=data[data[y].notna()], x=x, y=y, hue="region")
    plt.ylabel(ylabel)
    xtics = list(filter(lambda l: l>=np.nanmin(data[x].values), plt.xticks()[0]))
    plt.xticks(
        xtics,
        map(
            (lambda x: "+{:.0f}".format(x) if (float(x)>0) else "{:.0f}".format(x)),
            xtics
        ),
    )
    if pct:
        ytics = list(filter(lambda l: l>=np.nanmin(data[y].values), plt.yticks()[0]))
        plt.yticks(
            ytics,
            map(
                (lambda x: "{:.0f}%".format(x)),
                ytics
            ),
        )

def get_merged_df_with_x_days_before(dates, full_all_snapshots_df, days_before, snapshot_to_idx, progress_bar=False, plot_debug=False, mn=0):
    merged_df = None
    try:
        for region in df_regions.nom_regional.unique():
            all_snapshots_df = full_all_snapshots_df[full_all_snapshots_df["nom_regional"]==region]
            skipped = []
            for dat in tqdm(dates, desc=region, disable=not progress_bar):
                deaths, cases = analysis(all_snapshots_df, dat, days_before, snapshot_to_idx, plot=False)
                df = pd.merge(
                    deaths,
                    cases,
                )
                
                if df["obitos"].sum()<=0 or df["casos"].sum()<=0:
                    if progress_bar and plot_debug:
                        tqdm.write(
                            "Ignoring snapshot {snapshot} with {deaths} deaths and {cases} cases".format(
                                deaths=df["obitos"].sum(),
                                cases=df["casos"].sum(),
                                snapshot = dat,
                            )
                        )
                    skipped.append(dat)
                    continue
                if progress_bar and plot_debug:
                    tqdm.write(
                        "Snaphsot {snapshot} has {deaths} deaths and {cases} cases".format(
                            deaths=df["obitos"].sum(),
                            cases=df["casos"].sum(),
                            snapshot = dat,
                        )
                    )

                earliest_day = min(df["day"])

                df["earliest_day"] = earliest_day
                df["day"] = df["day"] - earliest_day
                df["region"] = region
                df["obitos_delta_pct"] = 1. * step_derivative_pct(df["obitos"].values)
                df["obitos_delta_pct"] = df["obitos_delta_pct"].replace([np.inf, -np.inf], np.nan)
                df["casos_delta_pct"] = 1. * step_derivative_pct(df["casos"].values)
                df["casos_delta_pct"] = df["casos_delta_pct"].replace([np.inf, -np.inf], np.nan)
                df["obitos_pct"] = integral_nonzero_start(df["obitos_delta_pct"].values, df["obitos"].values, 1.)
                df["casos_pct"] = integral_nonzero_start(df["casos_delta_pct"].values, df["casos"].values, 1.)
                merged_df = df if merged_df is None else pd.concat([merged_df, df])
            if progress_bar or plot_debug:
                tqdm.write(
                    "The following snaphsots were skipped: {skipped}".format(
                        skipped = skipped,
                    )
                )
    except KeyboardInterrupt:
        pass
    return merged_df


if __name__=="__main__":
    start_date = datetime.date(2020, 5, 31)
    end_date = datetime.date(2021, 2, 1)
    dates = list(daterange(start_date, end_date))
    snapshot_to_idx = {date:i for i,date in enumerate(dates)}

    df_regions = pd.read_csv("pop_and_regions.csv")

    all_snapshots_df = None
    for d in dates:
        ds = os.path.join(
            SNAPSHOT_FOLDER,
            "{date_str}_compilado.csv".format(
            date_str=d.strftime("%Y_%m_%d"),
            )
        )

        if os.path.exists(ds):
            df = (
                pd
                .read_csv(ds)[["data_ocorrencia","nom_municipio","casos","obitos"]]
                .join(df_regions[["nom_municipio","nom_regional"]].set_index("nom_municipio"), on="nom_municipio")
                .groupby(["data_ocorrencia","nom_regional"])
                .sum()[["casos","obitos"]]
            )
            df["dat_snapshot"] = d
            all_snapshots_df = df if all_snapshots_df is None else pd.concat([all_snapshots_df, df])

    all_snapshots_df = all_snapshots_df.reset_index()
    all_snapshots_df = all_snapshots_df[all_snapshots_df["data_ocorrencia"]!="IGNORADO"]
    all_snapshots_df["data_ocorrencia"] = pd.to_datetime(all_snapshots_df["data_ocorrencia"], format="%Y-%m-%d", errors="coerce")
    all_snapshots_df = all_snapshots_df.dropna()
    all_snapshots_df = all_snapshots_df[
        np.logical_and(
            all_snapshots_df["data_ocorrencia"]>=pd.to_datetime("2020-01-01"),
            all_snapshots_df["data_ocorrencia"]<pd.to_datetime("2021-02-01"),
        )
    ]

    retroactive_start_date = start_date + datetime.timedelta(days=7)

    for d in tqdm(list(daterange(retroactive_start_date, end_date, 7))):
        this_snapshots_df = all_snapshots_df[all_snapshots_df.dat_snapshot<=d]
        this_dates = list(daterange(start_date, d))


        merged_df = get_merged_df_with_x_days_before(this_dates, this_snapshots_df, 1, snapshot_to_idx, progress_bar=False)

        agg_df = pd.merge(
            merged_df.groupby(["day", "region"])[["obitos_pct","obitos_delta_pct","casos_pct","casos_delta_pct"]].mean(),
            merged_df.groupby(["day", "region"])[["obitos_pct","obitos_delta_pct","casos_pct","casos_delta_pct"]].std(),
            on=["day", "region"],
            suffixes=["_avg","_std"]
        ).reset_index()

        agg_df = agg_df[["day", "region", "obitos_delta_pct_avg", "casos_delta_pct_avg",]].fillna(0)

        agg_df = agg_df[agg_df["day"] <= 7]

        agg_df.to_csv("retroactive_data/retroactive_{date}.csv".format(date=d.strftime("%Y_%m_%d")), index=False)
