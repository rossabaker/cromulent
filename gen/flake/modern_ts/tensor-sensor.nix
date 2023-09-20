{ lib
, buildPythonPackage
, fetchPypi
}:

buildPythonPackage rec {
  pname = "tensor-sensor";
  version = "1.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-VAbHRqw3BqdfGhiz9+RBJcOZcNPN5UgPeL7xbqzyyTs=";
  };

  propagatedBuildInputs = [ ];

  pythonImportsCheck = [ "tensor.sensor" ];

  meta = with lib; {
    description = "The goal of this library is to generate more helpful exception messages for numpy/pytorch tensor algebra expressions";
    homepage = "https://github.com/parrt/tensor-sensor";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
