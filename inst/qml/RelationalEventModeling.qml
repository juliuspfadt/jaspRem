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
	infoBottom: qsTr(
			"**References**:  \n"
			+ "- Butts, C. T. (2008). A relational event framework for social action. Sociological Methodology, 38(1), 155–200.  \n"
			+ "- Meijerink-Bosman, M., Leenders, R., & Mulder, J. (2022). Dynamic relational event modeling: Testing, exploring, and applying. PLOS ONE, 17(8), Article e0272309. https://doi.org/10.1371/journal.pone.0272309.  \n"
			+ "- Perry, P. O., & Wolfe, P. J. (2013). Point process modelling for directed interaction networks. Journal of the Royal Statistical Society: Series B, 75(5), 821–849.  \n"
			+ "- Stadtfeld, C., & Block, P. (2017). Interactions, actors, and time: Dynamic network actor models. Social Networks, 50, 46–54.  \n  \n"
			+ "**R-packages**:  \n"
			+ "- remify  \n"
			+ "- remstats  \n"
			+ "- remstimate  \n"
	)

	VariablesForm
	{
		height: 420

		AvailableVariablesList {
			name: "allVariables"
			id: allVariables
		}

		AssignedVariablesList {
			name: "timeVariable"
			title: qsTr("Time Variable")
			allowedColumns: ["scale", "ordinal", "nominal"]
			singleVariable: true
			id: assignedVariableTime
			info: qsTr("Specifies the variable indicating the timing of each event. Required for ordering the event sequence in the relational event model. This is typically a numeric or time-stamp variable that represents when each event occurred.")
		}

		AssignedVariablesList {
			name: "actorVariableSender"
			title: qsTr("Actor Variable Sender")
			allowedColumns: ["scale", "ordinal", "nominal"]
			singleVariable: true
			info: qsTr("Specifies the variable identifying the sender (initiator) of each event. The sender corresponds to the actor who initiates the interaction in the event history.")
		}

		AssignedVariablesList {
			name: "actorVariableReceiver"
			title: qsTr("Actor Variable Receiver")
			allowedColumns: ["scale", "ordinal", "nominal"]
			singleVariable: true
			info: qsTr("Specifies the variable identifying the receiver (target) of each event. The receiver is the actor to whom the event is directed.")
		}

		AssignedVariablesList {
			name: "weightVariable"
			title: qsTr("Weight Variable")
			allowedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Optional. Specifies a numeric variable representing the weight or intensity of each event, for example, the strength, duration, or importance of an interaction. If present, event weights will be incorporated into the computation of endogenous statistics.")
		}

		AssignedVariablesList {
			name: "typeVariable"
			title: qsTr("Type Variable")
			allowedColumns: ["nominal"]
			singleVariable: true
			id: typeVar
			info: qsTr("Optional. Specifies a categorical variable indicating the type or category of each event (e.g., 'conflict', 'cooperation'). Including a type variable allows for modeling event heterogeneity in the analysis.")
		}

		CheckBox {
			id: syncAnalysisBox
			name: "syncAnalysisBox"
			label: qsTr("<b>Start/Sync Analysis</b>")
			checked: false
			info: qsTr("If checked, the analysis will automatically re-run whenever the data or variable assignments change.")
			Component.onCompleted: {
				background.color = "#ff8600"
			}
		}
	}

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
			info: qsTr("Upload a CSV or TXT file containing attributes for each actor (node-level covariates). These attributes can be used as exogenous variables in the relational event model.")
			rowComponent: RowLayout
			{
				FileSelector
				{
					name: "actorData"
					id: actorData
					label: ""
					placeholderText: qsTr("e.g., home/Data/actorData.csv")
					filter: "*.csv *.txt"
					save: false
					fieldWidth: 180 * preferencesModel.uiScale
				}
			}
		}

		ComponentsList
		{
			name: "dyadDataList"
			title: qsTr("Upload dyadic attributes")
			implicitHeight: 90 * preferencesModel.uiScale // about 3 rows
			minimumItems: 1
			info: qsTr("Upload a CSV or TXT file containing attributes for each dyad (pair-level covariates). These covariates describe properties or history of actor pairs and can be included as exogenous variables.")
			rowComponent: 
			RowLayout
			{
				FileSelector
				{
					id: dyadData
					name: "dyadData"
					label: ""
					placeholderText: qsTr("e.g., home/Data/dyadData.csv")
					filter: "*.csv *.txt"
					save: false
					fieldWidth: 180 * preferencesModel.uiScale
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
			info: qsTr("Choose the event modeling framework. 'Tie-oriented' treats events as changes in the tie between two actors. 'Actor-oriented' models the choices of actors to initiate events. See Butts (2008) and Stadtfeld & Block (2017) for details.")

			RadioButton
			{
				value: "tie"
				label: qsTr("Tie-oriented")
				checked: true
				info: qsTr("Choose this for tie-oriented (dyad-based) event modeling. Events are changes in the relationship (tie) between two actors, not individual choices.")

				RadioButtonGroup
				{
					name: "eventDirection"
					id: eventDirection
					title: ""
					radioButtonsOnSameRow: false
					info: qsTr("Choose whether events are modeled as directed (with sender and receiver) or undirected (no direction). Directed models are typical for communication, undirected for mutual events.")

					RadioButton
					{
						value: "directed"
						label: qsTr("Directed")
						checked: true
						info: qsTr("Select this if events have a direction (sender and receiver), e.g., emails or phone calls.")
					}
					RadioButton
					{
						value: "undirected"
						label: qsTr("Undirected")
						info: qsTr("Select this if events are mutual, with no direction (e.g., meetings, joint activities).")
					}
				}
			}
			RadioButton
			{
				value: "actor"
				label: qsTr("Actor-oriented")
				info: qsTr("Choose this for actor-oriented (actor-based) event modeling. Events represent choices made by individual actors to initiate interactions.")
			}
		}

		RadioButtonGroup
		{
			name: "eventSequence"
			title: qsTr("Event sequence")
			radioButtonsOnSameRow: false
			info: qsTr("Specify whether to use actual event timing ('Time sensitive') or only the event order. Use 'Time sensitive' for continuous-time models; 'Order only' if only event rank is meaningful.")

			RadioButton
			{
				value: "timeSensitive"
				label: qsTr("Time sensitive")
				checked: true
				info: qsTr("Model uses actual timing of events (event times are meaningful). Use for continuous-time analysis.")
			}
			RadioButton
			{
				value: "orderOnly"
				label: qsTr("Order only")
				info: qsTr("Model uses only the sequence order of events, ignoring exact timing. Use when only event rank matters.")
			}
		}

		RadioButtonGroup
		{
			name: "riskset"
			title: qsTr("Riskset")
			radioButtonsOnSameRow: false
			id: riskset
			info: qsTr("Specifies which dyads or actors are considered at risk for an event at each time point. 'Full' includes all possible pairs, 'Active' only current ones, 'Manual' allows custom exclusions.")

			RadioButton
			{
				value: "full"
				label: qsTr("Full")
				checked: true
				info: qsTr("All possible dyads (pairs) or actors are considered at risk for events at all time points.")
			}
			RadioButton
			{
				value: "active"
				label: qsTr("Active")
				info: qsTr("Only dyads or actors currently 'active' (e.g., present, not dissolved) are considered at risk for events.")
			}
			RadioButton
			{
				value: "manual"
				label: qsTr("Manual")
				info: qsTr("Manually specify which dyads or actors are excluded from the risk set at each time point using a file.")

				Label {
					text: qsTr("Upload dyads to exclude:")
					visible: riskset.value == "manual"
				}

				FileSelector
				{
					id: dyadExclude
					name: "dyadExclude"
					label: ""
					placeholderText: qsTr("e.g., home/Data/dyadExclude.csv")
					filter: "*.csv *.txt"
					save: false
					fieldWidth: 180 * preferencesModel.uiScale
					visible: riskset.value == "manual"
					info: qsTr(" Becomes visible when risket is chosen to be manual: Upload a CSV or TXT file listing dyads to be excluded from the risk set at any time point.")
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
		So first comes a list of endogenous variables that are needed later as sources for several ComponentsLists.
		If you wish to update and add effects, do that here.
		*/

		// The whole matched list of the effect variables R names and translations
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
		property var varsTieDirected: [
			"indegreeReceiver", "indegreeSender", "inertia", "isp", "itp", "osp", "otp",
			"outdegreeReceiver", "outdegreeSender", "psABAB", "psABAY", "psABBA", "psABBY", "psABXA",
			"psABXB", "psABXY", "recencyContinue", "recencyReceiveReceiver", "recencyReceiveSender",
			"recencySendReceiver", "recencySendSender", "reciprocity", "rrankReceive", "rrankSend",
			"totaldegreeDyad", "totaldegreeReceiver", "totaldegreeSender"
		];

		// variables for the tie undirected model
		property var varsTieUndirected: [
			"degreeDiff", "degreeMax", "degreeMin", "inertia", "psABAB",
			"psABAY", "recencyContinue", "sp", "totaldegreeDyad"
		];

		// variables for the actor receiver model
		property var varsActorReceiver: [
			"indegreeReceiver", "inertia", "isp", "itp", "osp", "otp",
			"outdegreeReceiver", "recencyContinue", "recencyReceiveReceiver",
			"recencySendReceiver", "reciprocity", "rrankReceive", "rrankSend",
			"totaldegreeReceiver", "psABAB", "psABAY", "psABBA", "psABBY", "psABXA",
			"psABXB", "psABXY"
		];

		// variables for the actor sender model
		property var varsActorSender: [
			"indegreeSender", "outdegreeSender", "recencySendSender", "recencyReceiveSender",
			"totaldegreeSender", "psABA", "psABB", "psABX"
		];

		// variables that only have two scaling arguments 
		property var varsScalingTwo: [
			"degreeDiff", "isp", "itp", "osp", "otp", "sp"
		];
		// variables that have no scaling arguments
		property var varsScalingNone: [
			"psABAB", "psABAY", "psABBA", "psABBY", "psABXA", "psABXB", "psABXY", "psABA", "psABB", "psABX",
			"recencyContinue", "recencyReceiveReceiver", "recencyReceiveSender", "recencySendReceiver", "recencySendSender", 
			"rrankReceive", "rrankSend"
		];

		// variables that do not have a consider-type argument 
		property var varsNotConsiderType: [
			"psABA", "psABB", "psABX"
		];

		// variables that have a unique argument
		property var varsUnique: [
			"isp", "itp", "osp", "otp", "sp"
		];

		// variables to use in the scaling column
		property var scalingTwo: [
			{label: qsTr("none"), value: "none"}, {label: qsTr("std"), value: "std"}
		];
		property var scalingAll: [
			{label: qsTr("none"), value: "none"}, {label: qsTr("prop"), value: "prop"}, {label: qsTr("std"), value: "std"}
		];

		// Main endogenous effects selection for the tie/receiver model
		Group 
		{
			title: orientation.value == "tie" ? "" : qsTr("Receiver Model")
			implicitHeight: 180 * preferencesModel.uiScale
			info: qsTr("Select endogenous network effects to include in the model. Endogenous effects describe how the event history (e.g., previous ties, degrees, reciprocation, recency) influences the likelihood of future events." 
			+ "These are computed from the event sequence and are model-internal. See https://tilburgnetworkgroup.github.io/remstats/reference/index.html#remstats for more info on endogenous effects.")

			ComponentsList 
			{ 
				implicitHeight: 150 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale
				name: "endogenousEffects"
				addItemManually: false
				id: endogenousEffects
				info: qsTr("Choose which endogenous effects to include in the tie-oriented or receiver model (visible in the actor-oriented model). "
				+ "You can select scaling and effect-specific options. Effects quantify patterns such as inertia, reciprocity, shared partners, and other network-driven event dependencies.")
				source: [{ 
						values: orientation.value == "tie" ? 
							(eventDirection.value == "undirected" ? effects.varsTieUndirected : effects.varsTieDirected) : 
							(effects.varsActorReceiver)
						}] 
				headerLabels: typeVar.count > 0 ? [qsTr("Include"), qsTr("Scaling"), qsTr("Consider type"), qsTr("Unique")]
					: [qsTr("Include"), qsTr("Scaling"), qsTr("Unique")]
				rowComponent: RowLayout {
					Text { Layout.preferredWidth: 180; text: effects.translated[rowValue] }
					TextField { name: "translatedName"; value: effects.translated[rowValue]; visible: false }
					CheckBox { name: "includeEndoEffect"; label: ""; Layout.preferredWidth: 60; id: inclEndoEff; info: qsTr("Tick to include this endogenous effect in the model.") }
					DropDown {
						name: "endogenousEffectsScaling"
						Layout.preferredWidth: 50
						values: effects.varsScalingTwo.includes(rowValue) ? effects.scalingTwo : effects.scalingAll
						enabled: !effects.varsScalingNone.includes(rowValue) & inclEndoEff.checked
						info: qsTr("Select the scaling method for this endogenous effect: 'none', 'proportion', or 'standardized'.")
					}
					DropDown {
						visible: typeVar.count > 0
						name: "endogenousEffectsConsiderType"
						Layout.preferredWidth: 70
						values: [{ label: qsTr("No"), value : "no"}, { label: qsTr("Yes"), value : "yes" }, { label: qsTr("Both"), value : "both" }]
						enabled: inclEndoEff.checked
						info: qsTr("For multi-type event models: specify whether the effect should be computed per event type, across types, or both.")
					}
					CheckBox {
						name: "endogenousEffectsUnique"
						Layout.preferredWidth: 60
						enabled: inclEndoEff.checked & effects.varsUnique.includes(rowValue)
						info: qsTr("If ticked, this effect will be treated as unique for this variable, depending on model specification.")
					}
				}
			}
		}

		// Endogenous effects for sender model (actor-oriented)
		Group 
		{
			visible: orientation.value == "actor"
			title: qsTr("Sender Model")
			implicitHeight: 130 * preferencesModel.uiScale
			info: qsTr("For the sender (actor-oriented) model, select endogenous effects. These effects capture how the sender's history and position in the network influence their event activity.")

			ComponentsList 
			{ 
				implicitHeight: 80 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale
				name: "endogenousEffectsSender"
				id: endogenousEffectsSender
				info: qsTr("For the sender in actor-oriented models, choose which endogenous effects to include. Effects reflect sender-based network dynamics.")
				source: [{ values: effects.varsActorSender }] 
				headerLabels: [qsTr("Include"), qsTr("Scaling"), qsTr("Consider type")]
				rowComponent: RowLayout {
					Text { Layout.preferredWidth: 200; text: effects.translated[rowValue] }
					TextField { name: "translatedNameSender"; value: effects.translated[rowValue]; visible: false; info: qsTr("Internal use: Translated effect label for R backend.") }
					CheckBox { name: "includeEndoEffectSender"; label: ""; Layout.preferredWidth: 80; id: inclEndoEffSend; info: qsTr("Tick to include this sender effect in the model.") }
					DropDown {
						name: "endogenousEffectsScalingSender"
						Layout.preferredWidth: 50
						values: effects.varsScalingTwo.includes(rowValue) ? effects.scalingTwo : effects.scalingAll
						enabled: !effects.varsScalingNone.includes(rowValue) & inclEndoEffSend.checked
						info: qsTr("Select the scaling method for this sender effect: 'none', 'proportion', or 'standardized'.")
					}
					CheckBox {
						name: "endogenousEffectsConsiderTypeSender"
						Layout.preferredWidth: 80
						visible: !effects.varsNotConsiderType.includes(rowValue)
						enabled: inclEndoEffSend.checked
						info: qsTr("Specify whether this effect should be computed per event type (if multiple types are modeled).")
					}
					CheckBox {
						name: "endogenousEffectsUniqueSender"
						visible: false
						info: qsTr("Internal use for aligning effect options with the R backend.")
					}
				}
			}
		}
	}

	Section
	{
		title: qsTr("Exogenous Effects")

		// Actor covariates (node-level)
		Group
		{
			Layout.columnSpan: 2
			ComponentsList
			{
				id: exogenousEffectsTableActors
				name: "exogenousEffectsTableActors"
				title: orientation.value == "tie" ? qsTr("Actor Effects") : qsTr("Actor Effects Receiver Model")
				info: qsTr("Select which actor-level covariates to include as exogenous effects in the model. Actor covariates are attributes such as gender, age, or other properties measured at the node level. You can choose how these effects are represented (e.g., as average, difference, maximum, minimum, etc.) depending on the model type.")
				headerLabels: orientation.value == "tie" ? (eventDirection.value == "directed" ? [qsTr("Average"), qsTr("Difference"), qsTr("Maximum"), qsTr("Minimum"), qsTr("Receive"), qsTr("Same"), qsTr("Send")] :
						[qsTr("Average"), qsTr("Difference"), qsTr("Maximum"), qsTr("Minimum"), qsTr("Same")]) :
					[qsTr("Average"), qsTr("Difference"), qsTr("Receive"), qsTr("Same")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 590 * preferencesModel.uiScale
				rSource: "exoTableVariablesActorsFromR"
				rowComponent: RowLayout {
					Text{Layout.preferredWidth: 110; text: rowValue}
					CheckBox {Layout.preferredWidth: 40; name: "average"; info: qsTr("Include the average value of this actor covariate across the dyad.") }
					CheckBox {Layout.preferredWidth: 50; name: "difference"; info: qsTr("Include the difference in value of this actor covariate between sender and receiver.") }
					CheckBox {Layout.preferredWidth: 45; name: "maximum"; visible: orientation.value == "tie"; info: qsTr("Include the maximum value of this actor covariate within the dyad.") }
					CheckBox {Layout.preferredWidth: 45; name: "minimum"; visible: orientation.value == "tie"; info: qsTr("Include the minimum value of this actor covariate within the dyad.") }
					CheckBox {Layout.preferredWidth: 40; name: "receive"; visible: !(orientation.value == "tie" && eventDirection.value == "undirected"); info: qsTr("Include the value of this covariate for the receiver actor.") }
					CheckBox {Layout.preferredWidth: 30; name: "same"; info: qsTr("Include an indicator if the actors in the dyad have the same value for this covariate.") }
					CheckBox {Layout.preferredWidth: 30; name: "send"; visible: orientation.value == "tie" && eventDirection.value == "directed"; info: qsTr("Include the value of this covariate for the sender actor.") }
					TextField { visible: false; name: "text1"; value: "average('" + rowValue + "')"; info: qsTr("Internal use: R code for average effect.") }
					TextField { visible: false; name: "text2"; value: "difference('" + rowValue + "')"; info: qsTr("Internal use: R code for difference effect.") }
					TextField { visible: false; name: "text4"; value: "maximum('" + rowValue + "')"; info: qsTr("Internal use: R code for maximum effect.") }
					TextField { visible: false; name: "text5"; value: "minimum('" + rowValue + "')"; info: qsTr("Internal use: R code for minimum effect.") }
					TextField { visible: false; name: "text6"; value: "receive('" + rowValue + "')"; info: qsTr("Internal use: R code for receiver effect.") }
					TextField { visible: false; name: "text7"; value: "same('" + rowValue + "')"; info: qsTr("Internal use: R code for same effect.") }
					TextField { visible: false; name: "text8"; value: "send('" + rowValue + "')"; info: qsTr("Internal use: R code for sender effect.") }
				}
			}
		}

		// Sender covariates (actor-oriented)
		Group
		{
			visible: orientation.value == "actor"
			title: qsTr("Actor Effects Sender Model")
			info: qsTr("For actor-oriented (sender) model, select which sender actor covariates to include as exogenous effects.")
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
					CheckBox {Layout.preferredWidth: 30; name: "send"; info: qsTr("Include the value of this covariate for the sender actor.") }
					TextField { visible: false; name: "text8"; value: "send('" + rowValue + "')"}
				}
			}
		}

		// Event-level covariates
		Group
		{
			ComponentsList
			{
				id: exogenousEffectsTableEvents
				name: "exogenousEffectsTableEvents"
				title: qsTr("Event Effect")
				info: qsTr("In the tie-oriented model, select event-level covariates to include as exogenous effects. Event covariates describe properties of the event itself, such as event type or other event-specific features.")
				visible: orientation.value == "tie"
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 290 * preferencesModel.uiScale
				rSource: "exoTableVariablesEventsFromR"
				rowComponent: RowLayout {
					Text{Layout.preferredWidth: 130; text: rowValue}
					CheckBox {Layout.preferredWidth: 35; name: "event"; info: qsTr("Include this event-level covariate as an exogenous effect.") }
					TextField { visible: false; name: "text3"; value: "event('" + rowValue + "')" }
				}
			}
		}



		// Dyad-level covariates
		Group
		{
			ComponentsList
			{
				id: exogenousEffectsTableDyads
				name: "exogenousEffectsTableDyads"
				title: qsTr("Tie Effect")
				info: qsTr("Select dyad-level (pair-specific) covariates to include as exogenous effects in the model. Dyadic covariates can be any variables measured for each pair of actors, such as geographical proximity or prior interactions.")
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 290 * preferencesModel.uiScale
				rSource: "exoTableVariablesDyadsFromR"
				rowComponent: RowLayout {
					Text{Layout.preferredWidth: 130; text: rowValue}
					CheckBox {Layout.preferredWidth: 30; name: "tie"; info: qsTr("Include this dyad-level covariate as an exogenous effect (tie effect).") }
					TextField { visible: false; name: "text9"; value: "tie('" + rowValue + "')" }
				}
			}
		}

		// Specified Effects Table (summary of chosen effects)
		Group
		{
			Layout.columnSpan: 2
			title: orientation.value == "tie" ? qsTr("Specified Effects") : qsTr("Specified Effects Receiver Model")
			info: qsTr("Displays and customizes the exogenous effects that have been specified, including scaling options and (for differences) the option to treat effects as absolute values.")
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
						name: "exogenousEffectsScaling"
						values: [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std" }]
						enabled: !(rowValue.startsWith("event") || rowValue.startsWith("same"))
						info: qsTr("Choose how to scale this exogenous effect: 'none' for unscaled, 'std' for standardized.")
					}
					CheckBox {
						Layout.preferredWidth: 100
						name: "exogenousEffectsAbsolute"
						visible: rowValue.startsWith("difference")
						info: qsTr("If checked, takes the absolute value of the difference when calculating this effect.")
					}
				}
			}
		}

		// Sender summary (actor-oriented)
		Group
		{
			visible: orientation.value == "actor"
			title: qsTr("Specified Effects Sender Model")
			info: qsTr("For the actor-oriented model, displays and customizes the sender exogenous effects that have been specified.")
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
						name: "exogenousEffectsScalingSender"
						values: [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std" }]
						enabled: !(rowValue.startsWith("event") || rowValue.startsWith("same"))
						info: qsTr("Choose how to scale this sender exogenous effect: 'none' for unscaled, 'std' for standardized.")
					}
					CheckBox {
						Layout.preferredWidth: 100
						name: "exogenousEffectsAbsoluteSender"
						visible: false
						info: qsTr("Internal use: Absolute value for sender difference (not shown).")
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
			info: qsTr("Select interaction effects to include in the model. Interaction effects allow you to model how the combination of two (or more) exogenous/endogenous jointly affects event likelihood. Common examples include interactions between actor and event covariates or between different actor covariates.")

			ComponentsList
			{
				name: "interactionEffects"
				rSource: "possibleInteractionEffectsFromR"
				// source: ["specifiedExogenousEffects", {name: "specifiedExogenousEffects", combineTerms: JASP.Combination2Way}]
				info: qsTr("List of possible interaction effects based on previously selected exogenous and edogenous effects.")
				headerLabels: [qsTr("Include")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 500 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text { Layout.preferredWidth: 400; text: rowValue }
					CheckBox { Layout.preferredWidth: 100; name: "includeInteractionEffect"; info: qsTr("Tick to include this interaction effect in the model.") }
				}
			}
		}

		Group
		{
			visible: orientation.value == "actor"
			title: qsTr("Sender Model")
			info: qsTr("For the actor-oriented model, select sender-based interaction effects to include in model. Sender interactions are typically between sender covariates and other exogenous variables.")

			ComponentsList
			{
				name: "interactionEffectsSender"
				rSource: "possibleInteractionEffectsFromRSender"
				// source: ["specifiedExogenousEffectsSender", {name: "specifiedExogenousEffectsSender", combineTerms: JASP.Combination2Way}]
				info: qsTr("List of possible sender-based interaction effects. Tick the checkbox to include an interaction effect in the sender model.")
				headerLabels: [qsTr("Include")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 500 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text { Layout.preferredWidth: 400; text: rowValue }
					CheckBox { Layout.preferredWidth: 100; name: "includeInteractionEffectSender"; info: qsTr("Tick to include this sender interaction effect in the model.") }
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
			info: qsTr("Select the estimation method for model parameters. Currently, only maximum likelihood estimation (MLE) is available, but Bayesian options may be added in the future.")

			RadioButtonGroup {
				name: "method"
				info: qsTr("Choose the estimation algorithm. MLE is standard for most applications.")
				RadioButton { value: "MLE"; label: qsTr("Maximum likelihood estimation"); checked: true }
				RadioButton { value: "BSIR"; label: qsTr("Bayesian importance resampling"); checked: false }
			}
		}

		Group 
		{
			title: qsTr("Statistics Options")
			info: qsTr("Configure how event statistics are calculated, including how to handle simultaneous events, the portion of the event history to use, and which time intervals to analyze.")

			RadioButtonGroup
			{
				name: "simultaneousEvents"
				title: qsTr("Simultaneous events")
				radioButtonsOnSameRow: true
				info: qsTr("Choose how to handle multiple events that occur at exactly the same time: join them into a single event ('Join') or treat each event separately ('Split').")
				RadioButton { label: qsTr("Join"); value: "join"; checked: true }
				RadioButton { label: qsTr("Split"); value: "split" }
			}

			DropDown {
				id: eventHistory
				label: qsTr("Consider event history")
				name: "eventHistory"
				values: [
					{ label: qsTr("Full"), value : "full"}, 
					{ label: qsTr("Window"), value : "window" },
					{ label: qsTr("Interval"), value : "interval" },
					{ label: qsTr("Decay"), value : "decay" }
				]
				info: qsTr("Specify how much of the event history is used when computing statistics. Options include using the full history, a rolling window, a specific interval, or applying temporal decay.")
			}

			IntegerField {
				visible: eventHistory.value == "window" || eventHistory.value == "decay"
				name: "eventHistorySingleInput"
				fieldWidth: 40
				label: eventHistory.value == "window" ? qsTr("     Time units in window") : qsTr("     Half-life time")
				defaultValue: 100
				info: qsTr("Set the size of the window (number of time units) or the half-life for temporal decay, depending on the chosen event history option.")
			}

			RowLayout 
			{
				visible: eventHistory.value == "interval"
				Label { text: qsTr("     Interval from") }
				IntegerField
				{
					name: "eventHistoryIntervalInputLower"
					label: ""
					defaultValue: 50
					min: 0
					fieldWidth: 40
					info: qsTr("Specify the lower and upper bound of the event history interval (in time units).")
				}
				Label { text: qsTr("to") }
				IntegerField
				{
					name: "eventHistoryIntervalInputUpper"
					label: ""
					defaultValue: 100
					min: 0
					fieldWidth: 40
				}
			}

			RowLayout
			{
				Label { text: qsTr("Compute statistics from timepoint") }
				IntegerField
				{
					name: "timepointInputLower"
					label: ""
					min: 1
					defaultValue: 1
					fieldWidth: 40
					info: qsTr("First and last time point to include in the computation of event statistics (use 'Inf' for all available data).")
				}
				Label { text: qsTr("to") }
				TextField
				{
					name: "timepointInputUpper"
					label: ""
					defaultValue: "Inf"
					fieldWidth: 40
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
			info: qsTr("Apply Bayesian regularization priors to the model parameters to prevent overfitting. Options include the horseshoe prior, Bayesian lasso, and Bayesian ridge. These approaches shrink regression coefficients and help with variable selection in high-dimensional models.")

			DropDown
			{
				name: "regularization"
				values: [
					{ label: qsTr("Horseshoe prior"), value : "horseshoe"}, 
					{ label: qsTr("Bayesian lasso"), value : "lasso" },
					{ label: qsTr("Bayesian ridge"), value : "ridge" }
				]
				addEmptyValue: true
				info: qsTr("Choose the type of Bayesian regularization prior for the model: Horseshoe (strong shrinkage, variable selection), Lasso (L1 penalty), or Ridge (L2 penalty). Leave empty for no regularization.")
			}

			CIField
			{
				text: qsTr("Credible interval")
				name: "regularizationCiLevel"
				info: qsTr("Set the credible interval (e.g., 95%) for Bayesian estimation of regularized effects.")
			}

			IntegerField
			{
				name: "regularizationIterations"
				label: qsTr("Iterations")
				defaultValue: 10000
				min: 1000
				fieldWidth: 70
				info: qsTr("Set the number of MCMC or optimization iterations for regularized estimation.")
			}

			CheckBox
			{
				name: "regularizationSetSeed"
				label: qsTr("Set seed")
				childrenOnSameRow: true
				info: qsTr("If checked, sets the random seed for reproducibility of the regularized estimation.")

				IntegerField
				{
					name: "regularizationSeed"
					label: ""
					defaultValue: 1234
					fieldWidth: 60
					min: 1
					max: 1e9
					info: qsTr("Specify the integer value for the random seed.")
				}
			}
		}

		Group 
		{

			info: qsTr("Select diagnostic plots to assess model fit and check for residual patterns. Plots include waiting time fit and residuals by effect.")

			CheckBox
			{
				name: "diagnosticPlots"
				id: diagPlots
				label: qsTr("Diagnostic Plots")
				info: qsTr("If checked, enables generation of diagnostic plots for model fit and residuals.")

				CheckBox
				{
					name: "diagnosticPlotWaitTime"
					label: qsTr("Waiting time fit")
					enabled: diagPlots.checked
					checked: true
					info: qsTr("Show a plot comparing observed and expected waiting times under the model.")
				}

				Text
				{
					text: qsTr("Residuals plot")
				}

				ComponentsList
				{
					id: residualSelect
					name: "residualPlotSelect"
					enabled: diagPlots.checked
					implicitHeight: 90 * preferencesModel.uiScale
					implicitWidth: 250 * preferencesModel.uiScale
					rSource: "effectsForPlot"
					info: qsTr("Select endogenous effects for which to plot model residuals.")
					rowComponent: RowLayout { 
						Text { Layout.preferredWidth: 300; text: rowValue }
						CheckBox { Layout.preferredWidth: 40; name: "includePlotEffect"; info: qsTr("Tick to include a residual plot for this effect.") }
					}
				}
			}
		}

		RowLayout 
		{
			CheckBox
			{
				name: "oldEffectsSaved"
				label: qsTr("Save old effects")
				checked: false
				info: qsTr("If checked, previously estimated effects will be saved internally to speed up computation. However, this can lead to memory issues with large models.")
			}
			HelpButton
			{
				toolTip: qsTr("Click for more information")
				helpPage: "forQml/tooltipSaveEffects"
			}
		}
	}

}
