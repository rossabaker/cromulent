{ lib
, buildPythonPackage
, fetchPypi
, numpy
, pandas
, patsy
, scikit-learn
, scipy
, statsmodels
}:

buildPythonPackage rec {
  pname = "category-encoders";
  version = "2.6.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-WeukYSfCzkzTHgF34KVXkgkLeyoOb1BReifh8iUddiI=";
  };

  propagatedBuildInputs = [
    numpy
    pandas
    patsy
    scikit-learn
    scipy
    statsmodels
  ];

  pythonImportsCheck = [ "category.encoders" ];

  meta = with lib; {
    description = "A collection of sklearn transformers to encode categorical variables as numeric";
    homepage = "https://github.com/scikit-learn-contrib/category_encoders";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
