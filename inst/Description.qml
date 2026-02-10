import QtQuick
import JASP.Module

Description
{
	name		: "jaspSyntheticData"
	title		: qsTr("Synthetic Data")
	description	: qsTr("A module to create synthetic data from a dataset")
	version		: "0.1"
	author		: "Erin M. Buchanan"
	maintainer	: "Erin M. Buchanan <buchananlab@gmail.com>"
	website		: "https://aggieerin.com"
	license		: "GPL (>= 2)"
	icon        : "jsdIcon.png" // Located in /inst/icons/
	preloadData: true
	requiresData: true

	Analysis
	{
	  title: qsTr("Synthetic Data")
	  menu: qsTr("Synthetic Data")
	  func: "syntheticData"        // Must match the R function name
	  qml: "SyntheticData.qml"     // The UI file for the analysis
	  requiresData: true
	}
}
