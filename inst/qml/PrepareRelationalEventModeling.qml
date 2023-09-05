import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{

	Group
	{
		title: qsTr("Load Data")

		RowLayout {
			FileSelector
			{
				id:										eventData
				name:									"eventData"
				label:								qsTr("Event data")
				placeholderText:			qsTr("e.g., home/Data/eventData.csv")
				filter:								"*.csv"
				save:									false
				fieldWidth:						180 * preferencesModel.uiScale
			}
			// Label {	text: qsTr("Show rows")}
			// IntegerField
			// {
			// 	name: 				"eventMinRow"
			// 	defaultValue:	1
			// 	min:					1
			// 	text:					qsTr("")
			// 	fieldWidth:		40
			// 	id: eventMinRow
			// }
			// Label {	text: qsTr("-")}
			// IntegerField
			// {
			// 	name: 				"eventMaxRow"
			// 	defaultValue:	5
			// 	min:					parseInt(eventMinRow.value) + 1
			// 	text:					qsTr("")
			// 	fieldWidth:		40
			// }
		}
		
		RowLayout
		{
			FileSelector
			{
			id:										personData
			name:									"personData"
			label:								qsTr("Person covariate data")
			placeholderText:			qsTr("e.g., home/Data/personData.csv")
			filter:								"*.csv"
			save:									false
			fieldWidth:						180 * preferencesModel.uiScale
			}
			// Label {	text: qsTr("Show rows")}
			// IntegerField
			// {
			// 	name: 				"personMinRow"
			// 	defaultValue:	1
			// 	min:					1
			// 	text:					qsTr("")
			// 	fieldWidth:		40
			// 	id: personMinRow
			// }
			// Label {	text: qsTr("-")}
			// IntegerField
			// {
			// 	name: 				"personMaxRow"
			// 	defaultValue:	5
			// 	min:					parseInt(parseMinRow.value) + 1
			// 	text:					qsTr("")
			// 	fieldWidth:		40
			// }
		}
		

	ComponentsList
	{
		name: "dyadDataList"
		title: qsTr("Dyadic covariate data")
		implicitHeight		: 90 * preferencesModel.uiScale // about 3 rows
		minimumItems: 1
		rowComponent: 
			RowLayout
			{
				FileSelector
				{
					id:										dyadData
					name:									"dyadData"
					label:								""
					placeholderText:			qsTr("e.g., home/Data/dyadData.csv")
					filter:								"*.csv"
					save:									false
					fieldWidth:						180 * preferencesModel.uiScale
				}
				// Label {	text: qsTr("Show rows")}
				// IntegerField
				// {
				// 	name: 				"dyadMinRow"
				// 	defaultValue:	1
				// 	min:					1
				// 	text:					qsTr("")
				// 	fieldWidth:		40
				// 	id: dyadMinRow
				// }
				// Label {	text: qsTr("-")}
				// IntegerField
				// {
				// 	name: 				"dyadMaxRow"
				// 	defaultValue:	5
				// 	min:					parseInt(dyadMinRow.value) + 1
				// 	text:					qsTr("")
				// 	fieldWidth:		40
				// }
			}
	}
		
		

	}

	Group{}
	Group{
		CheckBox
		{
			name: "mergeData"
			label: qsTr("Merge Data")
		}
	}
	Group{}
	Group
	{
		title: qsTr("Save Data")
		FileSelector
		{
			name:									"savePath"
			label:								qsTr("Save as")
			placeholderText:			qsTr("e.g., home/Data/mergedData.csv")
			filter:								"*.csv"
			save:									true
			fieldWidth:						180 * preferencesModel.uiScale
		}
	}
	

}
