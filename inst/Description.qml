import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspRem"
	title		: qsTr("REM")
	description	: qsTr("This module offers methods for relational event modeling.")
	version		: "0.17.2"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon      : "logo.png"
	hasWrappers	: false

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"logo.png"
	}

	Analysis
	{
	    title: 	"REM"
	    func: 	"frequentistRelationalEventModeling"
			qml: 		"FrequentistRelationalEventModeling.qml"
	}
	
	Separator {}

	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"logoBay.png"
	}

	Analysis
	{
	    title: 	"REM"
	    func: 	"bayesianRelationalEventModeling"
			qml: 		"BayesianRelationalEventModeling.qml"
	}
}
