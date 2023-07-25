{ lib
, buildPythonPackage
, fetchPypi
, jupyterlab
}:

buildPythonPackage rec {
  pname = "aquirdturtle-collapsible-headings";
  version = "3.1.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-XBd1U83kBDUVb/2pLHG647SPY12TE06jROJVe1I66iM=";
  };

  propagatedBuildInputs = [
    jupyterlab
  ];

  pythonImportsCheck = [ "aquirdturtle.collapsible.headings" ];

  meta = with lib; {
    description = "Make headings collapsible like the old Jupyter notebook extension and like mathematica notebooks";
    homepage = "https://github.com/aquirdTurtle/Collapsible_Headings.git";
    license = licenses.CHANGE;
    maintainers = with maintainers; [  ];
  };
}
