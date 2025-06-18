import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspRem"
	title		: qsTr("REM")
	description	: qsTr("This module offers methods for relational event modeling.")
	version		: "0.95.0"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon      : "logoFreq.png"
	hasWrappers	: false
	preloadData: true

	Analysis
	{
		title: 	"REM"
		func: 	"relationalEventModeling"
		qml: 		"RelationalEventModeling.qml"
		requiresData: true

	}



}


