{ lib
, buildPythonPackage
, fetchPypi
, numpy
, scipy
}:

buildPythonPackage rec {
  pname = "pymannkendall";
  version = "1.4.3";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-+eO7u1g7UoXRUIKqAAeCXlvqTd6YWNLnyoHubx43joI=";
  };

  propagatedBuildInputs = [
    numpy
    scipy
  ];

  pythonImportsCheck = [ "pymannkendall" ];

  meta = with lib; {
    description = "A python package for non-parametric Mann-Kendall family of trend tests";
    homepage = "https://github.com/mmhs013/pymannkendall";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
