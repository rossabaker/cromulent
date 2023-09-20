{ lib
, buildPythonPackage
, fetchPypi
, numba
, numpy
}:

buildPythonPackage rec {
  pname = "window-ops";
  version = "0.0.14";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-TA1GhsULofwd59Qdpwa1cRjY+0VTHloj8GEE+AR4jB0=";
  };

  propagatedBuildInputs = [
    numba
    numpy
  ];

  pythonImportsCheck = [ "window.ops" ];

  meta = with lib; {
    description = "Implementations of window operations such as rolling and expanding";
    homepage = "https://github.com/jmoralez/window_ops/tree/master/";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
