{ lib
, buildPythonPackage
, fetchPypi
, catboost
, holidays
, joblib
, lightgbm
, matplotlib
, nfoursid
, numpy
, pandas
, pmdarima
, prophet
, pyod
, requests
, scikit-learn
, scipy
, shap
, statsforecast
, statsmodels
, tbats
, tqdm
, xarray
, xgboost
}:

buildPythonPackage rec {
  pname = "u8darts";
  version = "0.24.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-KWFhSbVZRnqZchCRmtLkGRPXCBbWQ7tC+A4eRrSmOMM=";
  };

  propagatedBuildInputs = [
    catboost
    holidays
    joblib
    lightgbm
    matplotlib
    nfoursid
    numpy
    pandas
    pmdarima
    prophet
    pyod
    requests
    scikit-learn
    scipy
    shap
    statsforecast
    statsmodels
    tbats
    tqdm
    xarray
    xgboost
  ];

  pythonImportsCheck = [ "u8darts" ];

  meta = with lib; {
    description = "A python library for easy manipulation and forecasting of time series";
    homepage = "https://unit8co.github.io/darts/";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
