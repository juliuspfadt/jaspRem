import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{

	VariablesForm
	{
		preferredHeight: 300 * preferencesModel.uiScale
		AvailableVariablesList{	name:	"allVariables"; id: allVariables}
		AssignedVariablesList	{	name:	"timeVariable";			title: qsTr("Time Variable");		suggestedColumns: ["scale","ordinal", "nominal"];	singleVariable: true	; id: assignedVariableTime}
		AssignedVariablesList	{	name:	"actorVariables";		title: qsTr("Actor Variables");	suggestedColumns: ["scale","ordinal", "nominal"]; singleVariable: false; height: 75 * preferencesModel.uiScale}
		AssignedVariablesList	{	name:	"weightVariable";		title: qsTr("Weight Variable");	suggestedColumns: ["scale"];	singleVariable: true	}
		
		// AssignedVariablesList	{	name:	"covariates";				title: qsTr("Covariates");		suggestedColumns: ["scale","ordinal", "nominal"];	singleVariable: false	; height: 115 * preferencesModel.uiScale; id: covariates}
	}
	// // in order to have access to all variables in the data set even though they might not be assigned, 
// // this section is hidden
	Section
	{
		AssignedVariablesList{ name: "allVariablesHidden"; source: "allVariables" }
		visible: false
	}
	Section{
		title: qsTr("Upload Covariates Data")

		FileSelector
		{
		name:									"actorData"
		label:								qsTr("Upload actor attributes")
		placeholderText:			qsTr("e.g., home/Data/actorData.csv")
		filter:								"*.csv"
		save:									false
		fieldWidth:						180 * preferencesModel.uiScale
		}
		Group{}
		ComponentsList
		{
			name: "dyadDataList"
			title: qsTr("Upload dyadic attributes")
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
			}
		}

	}
	


	Section
	{
		title: qsTr("Model")
		columns: 2

		RadioButtonGroup
		{
			name: "orientation"
			id: orientation
			title: qsTr("Orientation")
			radioButtonsOnSameRow: false

			RadioButton
			{
				value: "tie"
				label: qsTr("Tie-oriented")
				checked: true

				RadioButtonGroup
				{
					name: "eventDirection"
					id: eventDirection
					title: qsTr("")
					radioButtonsOnSameRow: false

					RadioButton
					{
						value: "directed"
						label: qsTr("Directed")
						checked: true
					}
					RadioButton
					{
						value: "undirected"
						label: qsTr("Undirected")
					}
				}
			}
			RadioButton
			{
				value: "actor"
				label: qsTr("Actor-oriented")

				RadioButtonGroup
				{
					name: "actorDirection"
					id: actorDirection
					title: qsTr("")
					radioButtonsOnSameRow: false

					RadioButton
					{
						value: "sender"
						label: qsTr("Sender")
						checked: true
					}
					RadioButton
					{
						value: "receiver"
						label: qsTr("Receiver")
					}
				}
			}
		}



		RadioButtonGroup
		{
			name: "eventSequence"
			title: qsTr("Event sequence")
			radioButtonsOnSameRow: false

			RadioButton
			{
				value: "timeSensitive"
				label: qsTr("Time sensitive")
				checked: true
			}
			RadioButton
			{
				value: "orderOnly"
				label: qsTr("Order only")
			}
		}


		RadioButtonGroup
		{
			name: "riskset"
			title: qsTr("Riskset")
			radioButtonsOnSameRow: false

			RadioButton
			{
				value: "full"
				label: qsTr("Full")
				checked: true
			}
			RadioButton
			{
				value: "active"
				label: qsTr("Active")
			}
			// RadioButton
			// {
			// 	value: "manual"
			// 	label: qsTr("Manual")
			// }
		}
	}

 // section visible for the tie-oriented directed model
	Section 
	{
		title: qsTr("Effects")
		columns: 1

		Group 
		{
			title: qsTr("Endogenous effects")
			implicitHeight: 150 * preferencesModel.uiScale

			ComponentsList 
			{ 
				implicitHeight: 120 * preferencesModel.uiScale
				implicitWidth: 600 * preferencesModel.uiScale

				property var scalingTwo: [{label: qsTr("none"), value: "none"}, {label: qsTr("std"), value: "std"}]
				property var scalingAll: [{label: qsTr("none"), value: "none"}, {label: qsTr("prop"), value: "prop"}, {label: qsTr("std"), value: "std"}]
				
				property var varsScalingTwo: 
					["Degree difference", "Incoming shared partners", "Incoming two-path", "Outgoing shared partners", "Outgoing two-path", 
					"Shared partners"];
				property var varsScalingNone: 
					["Current common partner", "Pshift AB-AB", "Pshift AB-AY", "Pshift AB-BA", "Pshift AB-BY", "Pshift AB-XA", "Pshift AB-XB", "Pshift AB-XY", 
					"Recency continue", "Recency receive of receiver", "Recency receive of sender", "Recency send of receiver", "Recency send of sender", 
					"Recency rank receive", "Recency rank send", "User statistics"];
				
				// property var absolute: [""]
				property var varsNotConsiderType: ["Current common partner", "User statistics"]
				property var varsUnique:
					["Incoming shared partners", "Incoming two-path", "Outgoing shared partners", "Outgoing two-path", 
					"Shared partners"]
				

				rSource: "endoEffectsFromR"
				// source: [{ values: scalingNoneTieDirect.concat(scalingTwoTieDirect)}] 
				name: "endogenousEffects"
				id: endogenousEffects
				titles: ["", qsTr("Include"), qsTr("Scaling"), qsTr("Consider type"), qsTr("Unique")]
				rowComponent: RowLayout {
					Text{Layout.preferredWidth: 200; text: rowValue}
					CheckBox{ name: "includeEndoEffect"; label: ""; Layout.preferredWidth: 80; id: inclEndoEff}
					DropDown {
						name: "endogenousEffectsScaling"; 
						Layout.preferredWidth: 50
						values: endogenousEffects.varsScalingTwo.includes(rowValue) ? endogenousEffects.scalingTwo : endogenousEffects.scalingAll
						enabled: !endogenousEffects.varsScalingNone.includes(rowValue) & inclEndoEff.checked
					}
					CheckBox {
						name: "endogenousEffectsConsiderType"
						Layout.preferredWidth: 80
						visible: !endogenousEffects.varsNotConsiderType.includes(rowValue)
						enabled: inclEndoEff.checked
					}
					CheckBox {
						name: "endogenousEffectsUnique"
						Layout.preferredWidth: 50
						visible: endogenousEffects.varsUnique.includes(rowValue)
						enabled: inclEndoEff.checked
					}
				}
			}
}
			

		// VariablesForm
		// {
		// 	preferredHeight: 150 * preferencesModel.uiScale
		// 	AvailableVariablesList 
		// 	{ 
		// 		name: "possibleEndogenousEffects";
		// 		rSource: "endoEffectsFromR"
		// 		title: qsTr("Endogenous effects")
		// 	}


		// }

		Group
		{
			title: qsTr("Exogenous effects")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "exogenousEffectsTable"
				titles: ["Average", "Difference", "Event", "Maximum", "Minimum", "Receive", "Same", "Send", "Tie"]
				// maybe translate that?
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 600 * preferencesModel.uiScale
				rSource: "exoTableVariablesR"
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 130; text: rowValue } 
					CheckBox {Layout.preferredWidth: 40; name: "average"}
					CheckBox {Layout.preferredWidth: 50; name: "difference"}
					CheckBox {Layout.preferredWidth: 25; name: "event"}
					CheckBox {Layout.preferredWidth: 45; name: "maximum"}
					CheckBox {Layout.preferredWidth: 45; name: "minimum"}
					CheckBox {Layout.preferredWidth: 40; name: "receive"}
					CheckBox {Layout.preferredWidth: 30; name: "same"}
					CheckBox {Layout.preferredWidth: 30; name: "send"}
					CheckBox {Layout.preferredWidth: 30; name: "tie"}
				}
			}
		}

		Group
		{
			title: qsTr("Specified exogenous effects")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "specifiedExogenousEffects"
				id: specifiedExoEffects
				rSource: "specifiedEffectsFromR"

				titles: ["", qsTr("Scaling"), qsTr("Absolute")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 400 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 250; text: rowValue } 
					DropDown {
						name: "exogenousEffectsScaling"; 
						values: [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std" }]
						// values: (rowValue.startsWith("event") || rowValue.startsWith("same")) ? "" : [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std" }]

						enabled: !(rowValue.startsWith("event") || rowValue.startsWith("same"))
					}
					CheckBox {
						Layout.preferredWidth: 100
						name: "absolute"
						visible: rowValue.startsWith("difference")
					}
				}
			}
		}

		Group
		{
			title: qsTr("Interaction Effects")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "interactionEffects"
				rSource: "possibleInteractionEffectsFromR"
				titles: [qsTr("Include")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 500 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 400; text: rowValue } 
					CheckBox {Layout.preferredWidth: 100; name: "includeIA"}
				}
			}
		}
	}


	Section
	{
		title: qsTr("Estimation Options")

		RadioButtonGroup {
			title: qsTr("Estimation method")
			name: "method"
			RadioButton { value: "MLE" ; label: qsTr("Maximum likelihood estimation"); checked: true}
			RadioButton { value: "BSIR" ; label: qsTr("Bayesian importance resampling"); checked: false}
		}
	}

	Section
	{
		title: qsTr("Advanced Options")

		CheckBox
		{
			name:				"oldEffectsSaved"
			label:			qsTr("Save old effects")
			checked:		false
			info: 		qsTr("Something")
		}
	}

}


