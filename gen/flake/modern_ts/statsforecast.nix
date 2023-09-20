{ lib
, buildPythonPackage
, fetchPypi
, fugue
, matplotlib
, numba
, numpy
, pandas
, plotly
, plotly-resampler
, scipy
, statsmodels
, tqdm
}:

buildPythonPackage rec {
  pname = "statsforecast";
  version = "1.5.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-MZblKQjYokOdcy3En00/CunFmTviNiLM7dFSsWcb6AI=";
  };

  propagatedBuildInputs = [
    fugue
    matplotlib
    numba
    numpy
    pandas
    plotly
    plotly-resampler
    scipy
    statsmodels
    tqdm
  ];

  pythonImportsCheck = [ "statsforecast" ];

  meta = with lib; {
    description = "Time series forecasting suite using statistical models";
    homepage = "https://github.com/Nixtla/statsforecast/";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
