{ lib
, buildPythonPackage
, fetchPypi
, Sphinx
, gspread
, ipython
, numpy
, oauth2client
, pandas
, scipy
, setuptools
}:

buildPythonPackage rec {
  pname = "tsfel";
  version = "0.1.5";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-1B+kMicioj3ZJAezgUrT92CwAvPWNP4LFEoMvfGDTxg=";
  };

  propagatedBuildInputs = [
    Sphinx
    gspread
    ipython
    numpy
    oauth2client
    pandas
    scipy
    setuptools
  ];

  pythonImportsCheck = [ "tsfel" ];

  meta = with lib; {
    description = "Library for time series feature extraction";
    homepage = "https://github.com/fraunhoferportugal/tsfel/";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
