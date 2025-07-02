//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import JASP.Controls
import JASP	

Form
{

	VariablesForm
	{
		preferredHeight: 300 * preferencesModel.uiScale
		AvailableVariablesList{	name:	"allVariables"; id: allVariables}
		AssignedVariablesList	{	name:	"timeVariable";			title: qsTr("Time Variable");		allowedColumns: ["scale","ordinal", "nominal"];	singleVariable: true	; id: assignedVariableTime}
		AssignedVariablesList	{	name:	"actorVariableSender";		title: qsTr("Actor Variable Sender");	allowedColumns: ["scale","ordinal", "nominal"]; singleVariable: true}
		AssignedVariablesList	{	name:	"actorVariableReceiver";		title: qsTr("Actor Variable Receiver");	allowedColumns: ["scale","ordinal", "nominal"]; singleVariable: true}

		AssignedVariablesList	{	name:	"weightVariable";		title: qsTr("Weight Variable");	allowedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{	name:	"typeVariable";			title: qsTr("Type Variable");	allowedColumns: ["nominal"];	singleVariable: true; id:typeVar}

		CheckBox
		{
			id: 						syncAnalysisBox
			name: 					"syncAnalysisBox"
			label: 					qsTr("<b>Start/Sync Analysis</b>")
			checked: 				false
			Component.onCompleted:
			{
					background.color = "#ff8600"
			}
		}

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
					filter:								"*.csv *.txt"
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
					filter:								"*.csv *.txt"
					save:									false
					fieldWidth:						180 * preferencesModel.uiScale
				}
			}
		}

	}



	Section
	{
		title: qsTr("Model Options")
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
			id: riskset

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
			RadioButton
			{
				value: "manual"
				label: qsTr("Manual")
			
				Label {	text:	qsTr("Upload dyads to exclude:"); visible: riskset.value == "manual" }

				FileSelector
				{
					id:										dyadExclude
					visible: 							riskset.value == "manual"
					name:									"dyadExclude"
					label: 								""
					placeholderText:			qsTr("e.g., home/Data/dyadExclude.csv")
					filter:								"*.csv *.txt"
					save:									false
					fieldWidth:						180 * preferencesModel.uiScale
				}
			}


		}
	}

 // section visible for the tie-oriented directed model
	Section 
	{
		title: qsTr("Endogenous Effects")
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
			title: orientation.value == "tie" ? "" : qsTr("Receiver Model")
			implicitHeight: 180 * preferencesModel.uiScale

			ComponentsList 
			{ 
				implicitHeight: 150 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale

				source: [{ 
						values: orientation.value == "tie" ? 
							(eventDirection.value == "undirected" ? effects.varsTieUndirected : effects.varsTieDirected) : 
							(effects.varsActorReceiver)
						}] 
				name: "endogenousEffects"
				id: endogenousEffects
				headerLabels: typeVar.count > 0 ? [qsTr("Include"), qsTr("Scaling"), qsTr("Consider type"), qsTr("Unique")]:
																		[qsTr("Include"), qsTr("Scaling"), qsTr("Unique")]
				rowComponent: RowLayout {
					Text{Layout.preferredWidth: 180; text: effects.translated[rowValue]}
					// we need the invisible translated field to get the translated names into R
					TextField{ name: "translatedName"; value: effects.translated[rowValue]; visible: false}
					CheckBox{ name: "includeEndoEffect"; label: ""; Layout.preferredWidth: 60; id: inclEndoEff}
					DropDown {
						name: "endogenousEffectsScaling"; 
						Layout.preferredWidth: 50
						values: effects.varsScalingTwo.includes(rowValue) ? effects.scalingTwo : effects.scalingAll
						enabled: !effects.varsScalingNone.includes(rowValue) & inclEndoEff.checked
					}
					DropDown {
						visible: typeVar.count > 0
						name: "endogenousEffectsConsiderType"
						Layout.preferredWidth: 70
						values: [{ label: qsTr("No"), value : "no"}, { label: qsTr("Yes"), value : "yes" }, { label: qsTr("Both"), value : "both" }]
						enabled: inclEndoEff.checked
					}
					CheckBox {
						name: "endogenousEffectsUnique"
						Layout.preferredWidth: 60
						enabled: inclEndoEff.checked & effects.varsUnique.includes(rowValue)
					}
				}
			}
		}

		Group 
		{
			visible: orientation.value == "actor"
			title: qsTr("Sender Model")
			implicitHeight: 130 * preferencesModel.uiScale

			ComponentsList 
			{ 
				implicitHeight: 80 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale

				source: [{ values: effects.varsActorSender }] 
				name: "endogenousEffectsSender"
				id: endogenousEffectsSender
				headerLabels: [qsTr("Include"), qsTr("Scaling"), qsTr("Consider type")]
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
					// this checkbox is useful so that the options from this box align with the options from the above box in R
					CheckBox {
						name: "endogenousEffectsUniqueSender"
						visible: false
					}
				}
			}
		}
	}

	Section
	{
		title: qsTr("Exogenous Effects")

		Group 
		{
			Layout.columnSpan: 2
			ComponentsList
			{
				id: exogenousEffectsTableActors
				name: "exogenousEffectsTableActors"
				title: orientation.value == "tie" ? qsTr("Actor Effects") : qsTr("Actor Effects Receiver Model")
				headerLabels: orientation.value == "tie" ? (eventDirection.value == "directed" ? [qsTr("Average"), qsTr("Difference"), qsTr("Maximum"), qsTr("Minimum"), qsTr("Receive"), qsTr("Same"), qsTr("Send")] : 
						[qsTr("Average"), qsTr("Difference"), qsTr("Maximum"), qsTr("Minimum"), qsTr("Same")]) : 
					[qsTr("Average"), qsTr("Difference"), qsTr("Receive"), qsTr("Same")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale
				rSource: "exoTableVariablesActorsFromR"
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 110; text: rowValue} 
					CheckBox {Layout.preferredWidth: 40; name: "average"}
					CheckBox {Layout.preferredWidth: 50; name: "difference"}
					CheckBox {Layout.preferredWidth: 45; name: "maximum"; visible: orientation.value == "tie"}
					CheckBox {Layout.preferredWidth: 45; name: "minimum"; visible: orientation.value == "tie"}
					CheckBox {Layout.preferredWidth: 40; name: "receive"; visible: !(orientation.value == "tie" && eventDirection.value == "undirected")}
					CheckBox {Layout.preferredWidth: 30; name: "same"}
					CheckBox {Layout.preferredWidth: 30; name: "send"; visible: orientation.value == "tie" && eventDirection.value == "directed"}
					TextField { visible: false; name: "text1"; value: "average('" + rowValue + "')"}
					TextField { visible: false; name: "text2"; value: "difference('" + rowValue + "')"}
					TextField { visible: false; name: "text4"; value: "maximum('" + rowValue + "')"}
					TextField { visible: false; name: "text5"; value: "minimum('" + rowValue + "')"}
					TextField { visible: false; name: "text6"; value: "receive('" + rowValue + "')"}
					TextField { visible: false; name: "text7"; value: "same('" + rowValue + "')"}
					TextField { visible: false; name: "text8"; value: "send('" + rowValue + "')"}
				}
			}
		}

		Group
		{
			visible: orientation.value == "actor"
			title: qsTr("Actor Effects Sender Model")
			implicitHeight: 140 * preferencesModel.uiScale

			ComponentsList
			{
				id: exogenousEffectsTableSender
				name: "exogenousEffectsTableSender"
				headerLabels: [qsTr("Send")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 290 * preferencesModel.uiScale
				rSource: "exoTableVariablesActorsFromR"
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 180; text: rowValue} 
					CheckBox {Layout.preferredWidth: 30; name: "send"}
					TextField { visible: false; name: "text8"; value: "send('" + rowValue + "')"}
				}
			}
		}

		Group
		{
			ComponentsList
			{
				id: exogenousEffectsTableEvents
				name: "exogenousEffectsTableEvents"
				title: qsTr("Event Effect")
				visible: orientation.value == "tie"
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 290 * preferencesModel.uiScale
				rSource: "exoTableVariablesEventsFromR"
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 130; text: rowValue} 
					CheckBox {Layout.preferredWidth: 35; name: "event" }
					TextField { visible: false; name: "text3"; value: "event('" + rowValue + "')"}
				}
			}
		}

		Group
		{
			ComponentsList
			{
				id: exogenousEffectsTableDyads
				name: "exogenousEffectsTableDyads"
				title: qsTr("Tie Effect")
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 290 * preferencesModel.uiScale
				rSource: "exoTableVariablesDyadsFromR"
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 130; text: rowValue} 
					CheckBox {Layout.preferredWidth: 30; name: "tie"}
					TextField { visible: false; name: "text9"; value: "tie('" + rowValue + "')"}
				}
			}
		}

		Group
		{
			Layout.columnSpan: 2
			title: orientation.value == "tie" ? qsTr("Specified Effects") : qsTr("Specified Effects Receiver Model")
			implicitHeight: 140 * preferencesModel.uiScale

			ComponentsList
			{
				name: "specifiedExogenousEffects"
				id: specifiedExoEffects
				headerLabels: [qsTr("Scaling"), qsTr("Absolute")]
				// rSource: "specifiedExoEffectsFromR"
				source: [{name: "exogenousEffectsTableActors.text1", condition: "average"},
								 {name: "exogenousEffectsTableActors.text2", condition: "difference"},
								 {name: "exogenousEffectsTableEvents.text3", condition: "event"},
								 {name: "exogenousEffectsTableActors.text4", condition: "maximum"},
								 {name: "exogenousEffectsTableActors.text5", condition: "minimum"},
								 {name: "exogenousEffectsTableActors.text6", condition: "receive"},
								 {name: "exogenousEffectsTableActors.text7", condition: "same"},
								 {name: "exogenousEffectsTableActors.text8", condition: "send"},
								 {name: "exogenousEffectsTableDyads.text9", condition: "tie"}]

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
			title: qsTr("Specified Effects Sender Model")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "specifiedExogenousEffectsSender"
				id: specifiedExoEffectsSender
				// rSource: "specifiedExoEffectsFromRSender"
				source: [{name: "exogenousEffectsTableSender.text8", condition: "send"}]

				headerLabels: [qsTr("Scaling")]
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
	}

	Section
	{
		title: qsTr("Interaction Effects")
		Group
		{
			title: orientation.value == "tie" ? "" : qsTr("Receiver Model")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "interactionEffects"
				rSource: "possibleInteractionEffectsFromR"
				// source: ["specifiedExogenousEffects", {name: "specifiedExogenousEffects", combineTerms: JASP.Combination2Way}]
				headerLabels: [qsTr("Include")]
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
			title: qsTr("Sender Model")
			implicitHeight: 140 * preferencesModel.uiScale
			ComponentsList
			{
				name: "interactionEffectsSender"
				rSource: "possibleInteractionEffectsFromRSender"
				// source: ["specifiedExogenousEffectsSender", {name: "specifiedExogenousEffectsSender", combineTerms: JASP.Combination2Way}]
				headerLabels: [qsTr("Include")]
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

		Group 
		{
		title: qsTr("Estimation Method")
		visible: false // make this invisible to use at a later point, so now the method will always be MLE
			RadioButtonGroup {
				name: "method"
				RadioButton { value: "MLE" ; label: qsTr("Maximum likelihood estimation"); checked: true}
				RadioButton { value: "BSIR" ; label: qsTr("Bayesian importance resampling"); checked: false}
			}
		}


		Group 
		{
			title: qsTr("Statistics Options")

			RadioButtonGroup
			{
				name: "simultaneousEvents"
				title: qsTr("Simultaneous events")
				radioButtonsOnSameRow: true
				RadioButton { label: qsTr("Join"); value: "join"; checked: true}
				RadioButton { label: qsTr("Split"); value: "split" }
			}

			DropDown {
				id: eventHistory
				label: qsTr("Consider event history")
				name: "eventHistory";
				values: [{ label: qsTr("Full"), value : "full"}, 
							 	 { label: qsTr("Window"), value : "window" },
							 	 { label: qsTr("Interval"), value : "interval" },
							 	 { label: qsTr("Decay"), value : "decay" }]
			}

			IntegerField {
				visible: eventHistory.value == "window" || eventHistory.value == "decay"
				name: "eventHistorySingleInput"
				fieldWidth: 40
				label: eventHistory.value == "window" ? qsTr("     Time units in window") : qsTr("     Half-life time")
				defaultValue: 100
			}


			RowLayout 
			{
				visible: eventHistory.value == "interval"
				Label {	text: qsTr("     Interval from")}
				IntegerField
				{
					name:			"eventHistoryIntervalInputLower"
					label:			""
					defaultValue:	50
					min:			0
					fieldWidth: 	40
				}
				Label {	text: qsTr("to")}
				IntegerField
				{
					name:			"eventHistoryIntervalInputUpper"
					label:			""
					defaultValue:	100
					min:			0
					fieldWidth: 	40
				}
			}

			RowLayout
			{
				Label {	text: qsTr("Compute statistics from timepoint")}
				IntegerField
				{
					name:			"timepointInputLower"
					label:			""
					min:			1
					defaultValue: 1
					fieldWidth: 	40
				}
				Label {	text: qsTr("to")}
				TextField
				{
					name:			"timepointInputUpper"
					label:			""
					defaultValue: "Inf"
					fieldWidth: 	40
				}
			}
		}

	}

	Section
	{
		title: qsTr("Advanced Options")

		Group
		{
			title: qsTr("Regularization")
			DropDown
			{
				name: "regularization"
				values: [
					{ label: qsTr("Horseshoe prior"), value : "horseshoe"}, 
					{ label: qsTr("Bayesian lasso"), value : "lasso" },
					{ label: qsTr("Bayesian ridge"), value : "ridge" }
				]
				addEmptyValue: true
			}

			CIField {
				text: qsTr("Credible interval")
				name: "regularizationCiLevel"
			}

			IntegerField
			{
				name: "regularizationIterations"
				label: qsTr("Iterations")
				defaultValue: 10000
				min: 1000
				fieldWidth: 70
			}

			CheckBox
			{
				name: 				"regularizationSetSeed"
				label: 				qsTr("Set seed")
				childrenOnSameRow: 	true

				IntegerField
				{
					name: 			"regularizationSeed"
					label: 			""
					defaultValue: 	1234
					fieldWidth: 	60
					min: 			1
					max: 			1e9
				}
			}
		}

		Group 
		{
			CheckBox
			{
				name: "diagnosticPlots"
				id:diagPlots
				label: qsTr("Diagnostic Plots")
				CheckBox { name: "diagnosticPlotWaitTime"; label: qsTr("Waiting time fit"); enabled: diagPlots.checked; checked: true }
				// CheckBox { name: "diagnosticPlotResiduals"; label: qsTr("Residuals"); enabled: diagPlots.checked; checked: false; id: residualPlot}
				
				Text {text: qsTr("Residuals plot") }

				ComponentsList
				{
					id: residualSelect
					name: "residualPlotSelect"
					enabled: diagPlots.checked
					implicitHeight: 90 * preferencesModel.uiScale
					implicitWidth: 250 * preferencesModel.uiScale
					rSource: "effectsForPlot"
					rowComponent: RowLayout { 
						Text{Layout.preferredWidth: 300; text: rowValue} 
						CheckBox {Layout.preferredWidth: 40; name: "includePlotEffect"}
					}
				}
			}
		}

		
		RowLayout 
		{
			CheckBox
			{
				name:				"oldEffectsSaved"
				label:			qsTr("Save old effects")
				checked:		false
			}
			HelpButton
			{
				toolTip: 					qsTr("Click for more information")
				helpPage:					"forQml/tooltipSaveEffects"
			}
		}
	}
}
