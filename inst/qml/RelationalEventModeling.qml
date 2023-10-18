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

				// variables for the tie directed model
				property var varsTieDirected: ["indegreeReceiver", "indegreeSender", "inertia", "isp", "itp", "osp", "otp",
					"outdegreeReceiver", "outdegreeSender", "psABAB", "psABAY", "psABBA", "psABBY", "psABXA",
					"psABXB", "psABXY", "recencyContinue", "recencyReceiveReceiver", "recencyReceiveSender",
					"recencySendReceiver", "recencySendSender", "reciprocity", "rrankReceive", "rrankSend",
					"totaldegreeDyad", "totaldegreeReceiver", "totaldegreeSender", "userStat"];
				
				// variables for the tie undirected model
				property var varsTieUndirected: ["ccp", "degreeDiff", "degreeMax", "degreeMin", "inertia", "psABAB",
					"psABAY", "recencyContinue", "sp", "totaldegreeDyad", "userStat"];
				
				// variables for the actor sender model
				property var varsActorSender: ["indegreeSender", "outdegreeSender", "recencySendSender", "recencyReceiveSender",
					"totaldegreeSender", "userStat"];
				
				// variables for the actor receiver model
				property var varsActorReceiver: ["indegreeReceiver", "inertia", "isp", "itp", "osp", "otp",
					"outdegreeReceiver", "recencyContinue", "recencyReceiveReceiver",
					"recencySendReceiver", "reciprocity", "rrankReceive", "rrankSend",
					"totaldegreeReceiver", "userStat"];

				// thw whole matched list of the effect variables R names and translations
				property var translated: {
					"indegreeReceiver": qsTr("In degree receiver"),
					"indegreeSender": qsTr("In degree sender"), 
					"inertia": qsTr("Inertia"),
					"isp": qsTr("Incoming shared partners"), 
					"itp": qsTr("Incoming two-path"), 
					"osp": qsTr("Outgoing shared partners"),
					"otp": qsTr("Outgoing two-path"),
					"outdegreeReceiver": qsTr("Outdegree receiver"),
					"outdegreeSender": qsTr("Outdegree sender"),
					"psABAB": qsTr("Pshift AB-AB"),
					"psABAY": qsTr("Pshift AB-AY"),
					"psABBA": qsTr("Pshift AB-BA"),
					"psABBY": qsTr("Pshift AB-BY"),
					"psABXA": qsTr("Pshift AB-XA"),
					"psABXB": qsTr("Pshift AB-XB"),
					"psABXY": qsTr("Pshift AB-XY"),
					"recencyContinue": qsTr("Recency continue"), 
					"recencyReceiveReceiver": qsTr("Recency receive of receiver"), 
					"recencyReceiveSender": qsTr("Recency receive of sender"),
					"recencySendReceiver": qsTr("Recency send of receiver"),
					"recencySendSender": qsTr("Recency send of sender"),
					"reciprocity": qsTr("Reciprocity"),
					"rrankReceive": qsTr("Recency rank receive"),
					"rrankSend": qsTr("Recency rank send"),
					"totaldegreeDyad": qsTr("Total degree dyad"),
					"totaldegreeReceiver": qsTr("Total degree receiver"),
					"totaldegreeSender": qsTr("Total degree sender"),
					"userStat": qsTr("User statistics"),
					}

				// variables that only have two scaling arguments 
				property var varsScalingTwo: ["degreeDiff", "isp", "itp", "osp", "otp", "sp"];
				// variables that have no scaling arguments
				property var varsScalingNone: 
					["ccp", "psABAB", "psABAY", "psABBA", "psABBY", "psABXA", "psABXB", "psABXY", 
					"recencyContinue", "recencyReceiveReceiver", "recencyReceiveSender", "recencySendReceiver", "recencySendSender", 
					"rrankReceive", "rrankSend", "userStat"];

				// variables that do not have a consider-type argument 
				property var varsNotConsiderType: ["ccp", "userStat"]
				// variables that have a unique argument
				property var varsUnique: ["isp", "itp", "osp", "otp", "sp"]

				// variables to use in the scaling column
				property var scalingTwo: [{label: qsTr("none"), value: "none"}, {label: qsTr("std"), value: "std"}]
				property var scalingAll: [{label: qsTr("none"), value: "none"}, {label: qsTr("prop"), value: "prop"}, {label: qsTr("std"), value: "std"}]

				source: [
					{ 
						values: orientation.value == "tie" ? 
							(eventDirection.value == "undirected" ? varsTieUndirected : varsTieDirected) : 
							(actorDirection.value == "sender" ? varsActorSender : varsActorReceiver)
						}] 
				name: "endogenousEffects"
				id: endogenousEffects
				titles: ["", "", qsTr("Include"), qsTr("Scaling"), qsTr("Consider type"), qsTr("Unique")]
				rowComponent: RowLayout {
					Text{Layout.preferredWidth: 200; text: endogenousEffects.translated[rowValue]}
					TextField{ name: "translatedName"; value: endogenousEffects.translated[rowValue]; visible: false}
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
					Text{Layout.preferredWidth: 110; text: rowValue } 
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
				rSource: "specifiedExoEffectsFromR"

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


