{ lib
, buildPythonPackage
, fetchPypi
, graphviz
, matplotlib
, numpy
, pandas
, plotly
, scipy
, six
}:

buildPythonPackage rec {
  pname = "catboost";
  version = "1.2";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-OeU0A3J+z7tIFWdz7zAGtOccw1q0nMmgzwRLR0w0vgw=";
  };

  propagatedBuildInputs = [
    graphviz
    matplotlib
    numpy
    pandas
    plotly
    scipy
    six
  ];

  pythonImportsCheck = [ "catboost" ];

  meta = with lib; {
    description = "CatBoost Python Package";
    homepage = "https://catboost.ai";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
