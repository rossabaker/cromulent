{ lib
, buildPythonPackage
, fetchPypi
, matplotlib
, numpy
, scipy
, seaborn
}:

buildPythonPackage rec {
  pname = "missingno";
  version = "0.5.2";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-SkuqnKn55ODZQCRV3ya2VmMulLmeh/pkwM27vHIoN6w=";
  };

  propagatedBuildInputs = [
    matplotlib
    numpy
    scipy
    seaborn
  ];

  pythonImportsCheck = [ "missingno" ];

  meta = with lib; {
    description = "Missing data visualization module for Python";
    homepage = "https://github.com/ResidentMario/missingno";
    license = licenses.mit;
    maintainers = with maintainers; [  ];
  };
}
