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
		AssignedVariablesList	{	name:	"actorVariableSender";		title: qsTr("Actor Variable Sender");	suggestedColumns: ["scale","ordinal", "nominal"]; singleVariable: true}
		AssignedVariablesList	{	name:	"actorVariableReceiver";		title: qsTr("Actor Variable Receiver");	suggestedColumns: ["scale","ordinal", "nominal"]; singleVariable: true}

		AssignedVariablesList	{	name:	"weightVariable";		title: qsTr("Weight Variable");	suggestedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{	name:	"typeVariable";			title: qsTr("Type Variable");	suggestedColumns: ["nominal"];	singleVariable: true	}

		CheckBox
		{
			id: 						syncAnalysisBox
			name: 					"syncAnalysisBox"
			label: 					qsTr("<b>Sync Analysis</b>")
			checked: 				false
		}
		// Text {text: qsTr("You need to check this box for the analysis to run"); horizontalAlignment:	Text.AlignHCenter; verticalAlignment:		Text.AlignVCenter}

	}
	// // in order to have access to all variables in the data set even though they might not be assigned, 
// // this section is hidden
	Section
	{
		AssignedVariablesList{ name: "allVariablesHidden"; source: "allVariables" }
		visible: false
	}
	Section
	{
		columns: 1
		title: qsTr("Upload Covariates Data")


		ComponentsList
		{
			name: "actorDataList"
			title: qsTr("Upload actor attributes")
			implicitHeight: 90 * preferencesModel.uiScale // about 3 rows
			minimumItems: 1
			rowComponent: 
			RowLayout
			{
				FileSelector
				{
					name:									"actorData"
					id: 									actorData
					label:								""
					placeholderText:			qsTr("e.g., home/Data/actorData.csv")
					filter:								"*.csv"
					save:									false
					fieldWidth:						180 * preferencesModel.uiScale
				}
			}
		}
		


		ComponentsList
		{
			name: "dyadDataList"
			title: qsTr("Upload dyadic attributes")
			implicitHeight: 90 * preferencesModel.uiScale // about 3 rows
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
					title: ""
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
		id: effects
		/*
		So first comes a list of endogenous variables that are needed later as sources for several ComponentsLists
		If oyou wish to update and add effects, do that here
		*/

		// thw whole matched list of the effect variables R names and translations
		property var translated: {
			"indegreeReceiver": qsTr("Indegree receiver"),
			"indegreeSender": qsTr("Indegree sender"), 
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
			"psABA": qsTr("Pshift AB-A"), 
			"psABB": qsTr("Pshift AB-B"), 
			"psABX": qsTr("Pshift AB-X"),
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
			"degreeDiff": qsTr("Degree difference"),
			"degreeMax": qsTr("Degree maximum"),
			"degreeMin": qsTr("Degree minimum"), 
			"sp": qsTr("Shared partners")
			};

			// variables for the tie directed model
		property var varsTieDirected: ["indegreeReceiver", "indegreeSender", "inertia", "isp", "itp", "osp", "otp",
			"outdegreeReceiver", "outdegreeSender", "psABAB", "psABAY", "psABBA", "psABBY", "psABXA",
			"psABXB", "psABXY", "recencyContinue", "recencyReceiveReceiver", "recencyReceiveSender",
			"recencySendReceiver", "recencySendSender", "reciprocity", "rrankReceive", "rrankSend",
			"totaldegreeDyad", "totaldegreeReceiver", "totaldegreeSender"];
		
		// variables for the tie undirected model
		property var varsTieUndirected: ["degreeDiff", "degreeMax", "degreeMin", "inertia", "psABAB",
			"psABAY", "recencyContinue", "sp", "totaldegreeDyad"];
		
		// variables for the actor receiver model
		property var varsActorReceiver: ["indegreeReceiver", "inertia", "isp", "itp", "osp", "otp",
			"outdegreeReceiver", "recencyContinue", "recencyReceiveReceiver",
			"recencySendReceiver", "reciprocity", "rrankReceive", "rrankSend",
			"totaldegreeReceiver", "psABAB", "psABAY", "psABBA", "psABBY", "psABXA",
			"psABXB", "psABXY"];

		// variables for the actor sender model
		property var varsActorSender: ["indegreeSender", "outdegreeSender", "recencySendSender", "recencyReceiveSender",
			"totaldegreeSender", "psABA", "psABB", "psABX"];

		// variables that only have two scaling arguments 
		property var varsScalingTwo: ["degreeDiff", "isp", "itp", "osp", "otp", "sp"];
		// variables that have no scaling arguments
		property var varsScalingNone: 
			["psABAB", "psABAY", "psABBA", "psABBY", "psABXA", "psABXB", "psABXY", "psABA", "psABB", "psABX",
			"recencyContinue", "recencyReceiveReceiver", "recencyReceiveSender", "recencySendReceiver", "recencySendSender", 
			"rrankReceive", "rrankSend"];

		// variables that do not have a consider-type argument 
		property var varsNotConsiderType: ["psABA", "psABB", "psABX"]

		// variables that have a unique argument
		property var varsUnique: ["isp", "itp", "osp", "otp", "sp"]

		// variables to use in the scaling column
		property var scalingTwo: [{label: qsTr("none"), value: "none"}, {label: qsTr("std"), value: "std"}]
		property var scalingAll: [{label: qsTr("none"), value: "none"}, {label: qsTr("prop"), value: "prop"}, {label: qsTr("std"), value: "std"}]

		Group 
		{
			title: orientation.value == "tie" ? qsTr("Endogenous effects") : qsTr("Endogenous effects receiver model")
			implicitHeight: 150 * preferencesModel.uiScale

			ComponentsList 
			{ 
				implicitHeight: 120 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale

				source: [{ 
						values: orientation.value == "tie" ? 
							(eventDirection.value == "undirected" ? effects.varsTieUndirected : effects.varsTieDirected) : 
							(effects.varsActorReceiver)
						}] 
				name: "endogenousEffects"
				id: endogenousEffects
				titles: ["", "", qsTr("Include"), qsTr("Scaling"), qsTr("Consider type"), qsTr("Unique")]
				rowComponent: RowLayout {
					Text{Layout.preferredWidth: 180; text: effects.translated[rowValue]}
					TextField{ name: "translatedName"; value: effects.translated[rowValue]; visible: false}
					CheckBox{ name: "includeEndoEffect"; label: ""; Layout.preferredWidth: 60; id: inclEndoEff}
					DropDown {
						name: "endogenousEffectsScaling"; 
						Layout.preferredWidth: 50
						values: effects.varsScalingTwo.includes(rowValue) ? effects.scalingTwo : effects.scalingAll
						enabled: !effects.varsScalingNone.includes(rowValue) & inclEndoEff.checked
					}
					DropDown {
						name: "endogenousEffectsConsiderType"
						Layout.preferredWidth: 50
						values: [{ label: qsTr("No"), value : "no"}, { label: qsTr("Yes"), value : "yes" }, { label: qsTr("Both"), value : "both" }]
						enabled: inclEndoEff.checked
					}
					CheckBox {
						name: "endogenousEffectsUnique"
						Layout.preferredWidth: 60
						visible: effects.varsUnique.includes(rowValue)
						enabled: inclEndoEff.checked
					}
				}
			}
		}

		Group 
		{
			visible: orientation.value == "actor"
			title: qsTr("Endogenous effects sender model")
			implicitHeight: 130 * preferencesModel.uiScale

			ComponentsList 
			{ 
				implicitHeight: 80 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale



				source: [{ values: effects.varsActorSender }] 
				name: "endogenousEffectsSender"
				id: endogenousEffectsSender
				titles: ["", "", qsTr("Include"), qsTr("Scaling")]
				rowComponent: RowLayout {
					Text{Layout.preferredWidth: 200; text: effects.translated[rowValue]}
					TextField{ name: "translatedNameSender"; value: effects.translated[rowValue]; visible: false}
					CheckBox{ name: "includeEndoEffectSender"; label: ""; Layout.preferredWidth: 80; id: inclEndoEffSend}
					DropDown {
						name: "endogenousEffectsScalingSender"; 
						Layout.preferredWidth: 50
						values: effects.varsScalingTwo.includes(rowValue) ? effects.scalingTwo : effects.scalingAll
						enabled: !effects.varsScalingNone.includes(rowValue) & inclEndoEffSend.checked
					}
					CheckBox {
						name: "endogenousEffectsConsiderTypeSender"
						Layout.preferredWidth: 80
						visible: !effects.varsNotConsiderType.includes(rowValue)
						enabled: inclEndoEffSend.checked
					}
					// this checkbox is useful so that the options form this box align with the options from the above box in R
					CheckBox {
						name: "endogenousEffectsUniqueSender"
						visible: false
					}
				}
			}
		}

		Group
		{
			title: orientation.value == "tie" ? qsTr("Exogenous effects") : qsTr("Exogenous effects receiver model")
			implicitHeight: 150 * preferencesModel.uiScale

			ComponentsList
			{
				id: exogenousEffectsTable
				name: "exogenousEffectsTable"
				titles: orientation.value == "tie" ? (eventDirection.value == "directed" ? [qsTr("Average"), qsTr("Difference"), qsTr("Event"), qsTr("Maximum"), qsTr("Minimum"), qsTr("Receive"), qsTr("Same"), qsTr("Send"), qsTr("Tie")] : 
						[qsTr("Average"), qsTr("Difference"), qsTr("Event"), qsTr("Maximum"), qsTr("Minimum"), qsTr("Same"), "",  qsTr("Tie")]) : 
					[qsTr("Average"), qsTr("Difference"), "", qsTr("Receive"), qsTr("Same"), "", qsTr("Tie")]
				// maybe translate that?
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale
				rSource: "exoTableVariablesR"
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 110; text: rowValue} 
					CheckBox {Layout.preferredWidth: 40; name: "average"}
					CheckBox {Layout.preferredWidth: 50; name: "difference"}
					CheckBox {Layout.preferredWidth: 35; name: "event"; visible: orientation.value == "tie"  }
					CheckBox {Layout.preferredWidth: 45; name: "maximum"; visible: orientation.value == "tie"}
					CheckBox {Layout.preferredWidth: 45; name: "minimum"; visible: orientation.value == "tie"}
					CheckBox {Layout.preferredWidth: 40; name: "receive"; visible: !(orientation.value == "tie" && eventDirection.value == "undirected")}
					CheckBox {Layout.preferredWidth: 30; name: "same"}
					CheckBox {Layout.preferredWidth: 30; name: "send"; visible: orientation.value == "tie" && eventDirection.value == "directed"}
					CheckBox {Layout.preferredWidth: 30; name: "tie"}
					// TextField { visible: false; name: "text1"; value: rowValue}
					// TextField { visible: false; name: "text2"; value: rowValue}
					// TextField { visible: false; name: "text3"; value: rowValue + "event"}
					// TextField { visible: false; name: "text4"; value: rowValue + "maximum"}
					// TextField { visible: false; name: "text5"; value: rowValue + "minimum"}
					// TextField { visible: false; name: "text6"; value: rowValue + "receive"}
					// TextField { visible: false; name: "text7"; value: rowValue + "same"}
					// TextField { visible: false; name: "text8"; value: rowValue + "send"}
					// TextField { visible: false; name: "text9"; value: rowValue + "tie"}
				}
			}
		}

		Group
		{
			visible: orientation.value == "actor"
			title: qsTr("Exogenous effects sender model")
			implicitHeight: 140 * preferencesModel.uiScale

			ComponentsList
			{
				id: exogenousEffectsTableSender
				name: "exogenousEffectsTableSender"
				titles: ["", "", "", "", "", "", "", "", qsTr("Send")]
				// maybe translate that?
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 400 * preferencesModel.uiScale
				rSource: "exoTableVariablesR"
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 140; text: rowValue} 
					CheckBox {Layout.preferredWidth: 40; name: "average"; visible: false}
					// TextField { visible: false; name: "text1"; value: rowValue + "average"}
					CheckBox {Layout.preferredWidth: 50; name: "difference"; visible: false}
					CheckBox {Layout.preferredWidth: 25; name: "event"; visible: false}
					CheckBox {Layout.preferredWidth: 45; name: "maximum"; visible: false}
					CheckBox {Layout.preferredWidth: 45; name: "minimum"; visible: false}
					CheckBox {Layout.preferredWidth: 40; name: "receive"; visible: false}
					CheckBox {Layout.preferredWidth: 30; name: "same"; visible: false}
					CheckBox {Layout.preferredWidth: 30; name: "send"}
					CheckBox {Layout.preferredWidth: 30; name: "tie"; visible: false}
				}
			}
		}

		Group
		{
			title: orientation.value == "tie" ? qsTr("Specified exogenous effects") : qsTr("Specified exogenous effects receiver model")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "specifiedExogenousEffects"
				id: specifiedExoEffects
				rSource: "specifiedExoEffectsFromR"
				// source: [{name: "exogenousEffectsTable.text1", condition: "average"},
				// 				 {name: "exogenousEffectsTable.text2", condition: "difference"},
				// 				 {name: "exogenousEffectsTable.text3", condition: "event"},
				// 				 {name: "exogenousEffectsTable.text4", condition: "maximum"},
				// 				 {name: "exogenousEffectsTable.text5", condition: "minimum"},
				// 				 {name: "exogenousEffectsTable.text6", condition: "receive"},
				// 				 {name: "exogenousEffectsTable.text7", condition: "same"},
				// 				 {name: "exogenousEffectsTable.text8", condition: "send"},
				// 				 {name: "exogenousEffectsTable.text9", condition: "tie"}]

				titles: orientation.value == "tie" ? ["", qsTr("Scaling"), qsTr("Absolute")] : [qsTr("Scaling"), qsTr("Absolute")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 400 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 250; text: rowValue} 
					DropDown {
						name: "exogenousEffectsScaling"; 
						values: [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std" }]
						// values: (rowValue.startsWith("event") || rowValue.startsWith("same")) ? "" : [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std" }]
						enabled: !(rowValue.startsWith("event") || rowValue.startsWith("same"))
					}
					CheckBox {
						Layout.preferredWidth: 100
						name: "exogenousEffectsAbsolute"
						visible: rowValue.startsWith("difference")
					}
				}
			}
		}

		Group
		{
			visible: orientation.value == "actor"
			title: qsTr("Specified exogenous effects sender model")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "specifiedExogenousEffectsSender"
				id: specifiedExoEffectsSender
				rSource: "specifiedExoEffectsFromRSender"
				// source: [{name: "exogenousEffectsTableSender.text1", condition: "average == true"}]

				titles: qsTr("Scaling")
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 400 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text { Layout.preferredWidth: 250; text: rowValue } 
					DropDown {
						name: "exogenousEffectsScalingSender"; 
						values: [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std" }]
						// values: (rowValue.startsWith("event") || rowValue.startsWith("same")) ? "" : [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std" }]
						enabled: !(rowValue.startsWith("event") || rowValue.startsWith("same"))
					}
					CheckBox {
						Layout.preferredWidth: 100
						name: "exogenousEffectsAbsoluteSender"
						visible: false
					}
				}
			}
		}

		Group
		{
			title: orientation.value == "tie" ? qsTr("Interaction effects") : qsTr("Interaction effects receiver model")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "interactionEffects"
				rSource: "possibleInteractionEffectsFromR"
				// source: ["specifiedExogenousEffects", {name: "specifiedExogenousEffects", combineTerms: JASP.Combination2Way}]
				titles: [qsTr("Include")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 500 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 400; text: rowValue } 
					CheckBox {Layout.preferredWidth: 100; name: "includeInteractionEffect"}
				}
			}
		}

		Group
		{
			visible: orientation.value == "actor"
			title: qsTr("Interaction effects sender model")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "interactionEffectsSender"
				rSource: "possibleInteractionEffectsFromRSender"
				// source: ["specifiedExogenousEffectsSender", {name: "specifiedExogenousEffectsSender", combineTerms: JASP.Combination2Way}]
				titles: [qsTr("Include")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 500 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 400; text: rowValue } 
					CheckBox {Layout.preferredWidth: 100; name: "includeInteractionEffectSender"}
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