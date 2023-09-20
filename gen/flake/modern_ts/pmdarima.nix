{ lib
, buildPythonPackage
, fetchPypi
, Cython
, joblib
, numpy
, pandas
, scikit-learn
, scipy
, setuptools
, statsmodels
, urllib3
}:

buildPythonPackage rec {
  pname = "pmdarima";
  version = "2.0.3";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-kayz98EvPP1iY+/jI9O4+UtY3NJC1o8RVgpscXABsB8=";
  };

  propagatedBuildInputs = [
    Cython
    joblib
    numpy
    pandas
    scikit-learn
    scipy
    setuptools
    statsmodels
    urllib3
  ];

  pythonImportsCheck = [ "pmdarima" ];

  meta = with lib; {
    description = "Python's forecast::auto.arima equivalent";
    homepage = "http://alkaline-ml.com/pmdarima";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
