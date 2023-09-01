import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspRem"
	title		: qsTr("REM")
	description	: qsTr("This module offers methods for relational event modeling.")
	version		: "0.17.3"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon      : "logoFreq.png"
	requiresData: false
	hasWrappers	: false

	// GroupTitle
	// {
	// 	title:	qsTr("")
	// 	icon:	"logo.png"
	// }
	Analysis
	{
		title: 	"Prepare REM"
		func: 	"prepareRelationalEventModeling"
		qml: 		"PrepareRelationalEventModeling.qml"
		// icon: 	"logoBay.png"
	}

	Separator {}

	Analysis
	{
		title: 	"REM"
		func: 	"relationalEventModeling"
		qml: 		"RelationalEventModeling.qml"
		requiresData: true

			// icon: 	"logoFreq.png"
	}
	

}



	// GroupTitle
	// {
	// 	title:	qsTr("")
	// 	icon:	"logoBay.png"
	// }


