import os
import subprocess
import shutil
import datetime
import pandas as pd

SNAPSHOT_FOLDER = "big_test_snapshots_diarios"

def daterange(start_date, end_date, step=None):
  for n in range(0,int((end_date - start_date).days),step):
    yield start_date + datetime.timedelta(days=n)
        
def clean_deaths():
  df = pd.read_csv("deaths_unclean.csv")

  # Get only the most common regions
  g = df.groupby(["nom_municipio","nom_regional"]).count()
  cities_regions = {c:("",0) for c in df["nom_municipio"].unique()}
  for c,r in g.index:
    count = g.loc[(c,r)]["data_ocorrencia"]
    if cities_regions[c][1] < count:
      cities_regions[c] = (r,count)

  for c in cities_regions:
    df.loc[df["nom_municipio"]==c,"nom_regional"] = cities_regions[c][0]

  # Replace 2002 with 2020 since it is a common typo
  df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2002-","2020-")
  df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2022-","2020-")
  df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2200-","2020-")
  df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2202-","2020-")
  df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2220-","2020-")
  df["data_ocorrencia"] = df["data_ocorrencia"].str.replace("2222-","2020-")
  df[df.data_ocorrencia.isna()] = "IGNORADO"

  df.loc[df["data_ocorrencia"].str.startswith("201"),"data_ocorrencia"] = "2020-01-01"
  df.loc[df["data_ocorrencia"].str.startswith("200"),"data_ocorrencia"] = "2020-01-01"

  df.to_csv("deaths.csv")

def copy_cases_file(date):
  fname = "{date_str}_compilado.csv".format(
    date_str=date.strftime("%Y_%m_%d"),
  )
  shutil.copy(os.path.join(SNAPSHOT_FOLDER,fname), "deaths_unclean.csv")

def gen_onset_to_death(date):
  fname = "{date_str}_onset_to_death.csv".format(
    date_str=date.strftime("%Y_%m_%d"),
  )
  df = pd.read_csv(os.path.join(SNAPSHOT_FOLDER,fname))
  count = df.data_onset_death.count()
  df = df[df.data_onset_death>=0]
  pd.DataFrame(
    {
      "count": [count],
      "avg_days": [df.data_onset_death.mean()],
      "std_days": [df.data_onset_death.std()],
      "coeff_variation": [df.data_onset_death.std()/df.data_onset_death.mean()]
    }
  ).to_csv("onset_to_death.csv", index=False)

def prepare_google_mobility(date):
  fname = "Global_Mobility_Report.csv"
  df = pd.read_csv(os.path.join(SNAPSHOT_FOLDER,fname), low_memory=False)
  df.date = pd.to_datetime(df.date, format="%Y-%m-%d")
  last_mobility_datetime = datetime.datetime(date.year,date.month,date.day) - datetime.timedelta(days=7)
  df = df[df.date<last_mobility_datetime]
  #TODO: Convert date back to str
  df.to_csv(fname, index=False)

START_OPTS = "-m DEVELOP -k 7 -i 3000 -w 2400 -c 4 -t 8 -r '{date_str}' -v TRUE >'{date_str}.out'"
SUBSEQ_OPTS = "-m DEVELOP -k 7 -i 1200 -w 600 -c 4 -t 8 -r '{date_str}' -e {previous_model_file} -v TRUE >'{date_str}.out'"

if __name__=="__main__":
  start_date = datetime.date(2020, 5, 31)
  end_date = datetime.date(2021, 2, 1)

  opts = START_OPTS
  last_model_path = None

  for data_date in daterange(start_date, end_date, 7):
      reference_date = data_date + datetime.timedelta(days=1)
      print(reference_date.strftime("%Y-%m-%d"))
      # Copy data files
      prepare_google_mobility(data_date)
      gen_onset_to_death(data_date)
      copy_cases_file(data_date)
      clean_deaths()
      # Run model
      subprocess.call(
      #os.system(
        'Rscript run_regions.R {opts}'.format(
          opts = opts.format(
            date_str = reference_date.strftime("%Y-%m-%d"),
            previous_model_file = last_model_path,
          )
        ),
        shell=True
      )
      # Get saved model path
      results_path = "../results/{date_str}".format(
        date_str = reference_date.strftime("%Y_%m_%d"),
      )
      last_model_path = os.path.join(
        results_path,
        os.listdir(results_path)[0],
      )
      print(last_model_path)
      # Ensure we will be loading the previous model on the next step
      opts = SUBSEQ_OPTS
