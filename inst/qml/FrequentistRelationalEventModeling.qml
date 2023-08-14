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
		AssignedVariablesList	{	name:	"timeVariable";			title: qsTr("Time Variable");		suggestedColumns: ["scale"];	singleVariable: true	; id: assignedVariableTime}
		AssignedVariablesList	{	name:	"actorVariables";		title: qsTr("Actor Variables");	suggestedColumns: ["scale","ordinal", "nominal"]; singleVariable: false; height: 75 * preferencesModel.uiScale}
		AssignedVariablesList	{	name:	"weightVariable";		title: qsTr("Weight Variable");	suggestedColumns: ["scale"];	singleVariable: true	}
		
		// AssignedVariablesList	{	name:	"covariates";				title: qsTr("Covariates");		suggestedColumns: ["scale","ordinal", "nominal"];	singleVariable: false	; height: 115 * preferencesModel.uiScale; id: covariates}

	}

// // in order to have access to all variables in the data set even though they might not be assigned, 
// // this section is hidden
// 	Section
// 	{
// 		AssignedVariablesList{ name: "allVariablesHidden"; source: "allVariables" }
// 		visible: true
// 	}

	Section
	{
		title: qsTr("Model")
		columns: 2

		RadioButtonGroup
		{
			name: "eventDirection"
			id: eventDirection
			title: qsTr("Event direction")
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
			name: "orientation"
			id: orientation
			title: qsTr("Orientation")
			radioButtonsOnSameRow: false

			RadioButton
			{
				value: "tie"
				label: qsTr("Tie-oriented")
				checked: true
			}
			RadioButton
			{
				value: "actor"
				label: qsTr("Actor-oriented")
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
			RadioButton
			{
				value: "manual"
				label: qsTr("Manual")
			}
		}
	}

 // section visible for the tie-oriented directed model
	Section 
	{
		title: qsTr("Endogenous Effects")
		columns: 1

		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList 
			{ 
				name: "possibleEndogenousEffects";
				rSource: "endoEffectsFromR"
				title: qsTr("Endogenous effects")
			}

			AssignedVariablesList 
			{ 
				id: specifiedEndogenousEffects
				name: "specifiedEndogenousEffects"

				property var scalingTwo: [{label: qsTr("none"), value: "none"}, {label: qsTr("std"), value: "std"}]
				property var scalingAll: [{label: qsTr("none"), value: "none"}, {label: qsTr("prop"), value: "prop"}, {label: qsTr("std"), value: "std"}]
				property var scalingTwoVars: 
					["Degree difference", "Outgoing two-path", "Incoming two-path", "Outgoing shared partners", "Incoming shared partners", 
					 "Shared partners"];
				property var scalingNoneVars: ["Pshift AB-AB", "Pshift AB-AY", "Pshift AB-BA", "Pshift AB-BY", "Pshift AB-XA", "Pshift AB-XB", "Pshift AB-XY", 
																	 "Recency continue", "Recency receive of receiver", "Recency send of receiver", "Recency send of sender", 
																	 "Recency rank receive", "Recency rank send"];

				rowComponent: DropDown {
					name: "endogenousEffectScaling"; 
					values: specifiedEndogenousEffects.scalingTwoVars.includes(rowValue) ? specifiedEndogenousEffects.scalingTwo : specifiedEndogenousEffects.scalingAll 
					visible: !specifiedEndogenousEffects.scalingNoneVars.includes(rowValue)
					}
			}
		}
	}

	Section
	{
		title: qsTr("Exogenous Effects")
		columns: 1
		visible: orientation.value == "tie" && eventDirection.value == "directed"

		TableView
		{

			id: exoEffectsTable
			modelType			: JASP.Simple

			implicitWidth		: form.implicitWidth
			implicitHeight		: 200 * preferencesModel.uiScale // about 3 rows

			initialRowCount		: allVariables.intValue 
			initialColumnCount	: 9

			// rowCount			: allVariables.intValue
			// columnCount			: 9

			name				: "exogenousEffectsTable"
			title: qsTr("Tab")
			cornerText			: qsTr("Variables")
			columnNames			: [qsTr("Average"), qsTr("Difference"), qsTr("Event"), qsTr("Maximum"), qsTr("Minimum"), qsTr("Receive"), qsTr("Same"), qsTr("Send"), qsTr("Tie")]
			isFirstColEditable	: true
			itemType			: JASP.Integer
			// source: [{["allVariables", "covariates"], discard: [{values: ["name", "time_y"]}] }]
			source: [{name: "allVariables", discard: [{values: ["name", "time_y"]}]}]

			function getDefaultValue(columnIndex, rowIndex)				{ return 0	}
		}

		Group
		{
			title: qsTr("Specified Effects")
			implicitHeight: 150 * preferencesModel.uiScale

			ComponentsList
			{

				name: "specifiedExogenousEffects"
				id: specifiedExoEffects

				rSource: "specifiedEffectsFromR"
				property var scalingNoneVars: ["event", "same"];
				property var absoluteVars: ["difference"];


				titles: [qsTr("Scaling")
				// , qsTr("Absolute")
				]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 400 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 250; text: rowValue } 
					DropDown {
						name: "exoEffectsScaling"; 
						values: [{ label: qsTr("none"), value : "none"}, { label: qsTr("std"), value : "std"}]
						// visible: !specifiedExoEffects.scalingNoneVars.includes(rowValue)
					}
					// CheckBox {
					// 	Layout.preferredWidth: 100; 
					// 	name: "absolute"; 
					// 	// visible: specifiedExoEffects.absoluteVars.includes(rowValue)
					// }
				}
			}
		}

		Group
		{
			title: qsTr("Interaction Effects")
			implicitHeight: 150 * preferencesModel.uiScale
			ComponentsList
			{
				name: "interactionEffects"
				rSource: "possibleInteractionEffectsFromR"
				titles: [qsTr("Include")]
				implicitHeight: 100 * preferencesModel.uiScale
				implicitWidth: 500 * preferencesModel.uiScale
				rowComponent: RowLayout { 
					Text{Layout.preferredWidth: 400; text: rowValue } 
					CheckBox {Layout.preferredWidth: 100; name: "include"}
				}
			}
		}
	}


//  // section visible for the tie-oriented undirected model
// 	Section 
// 	{
// 		title: qsTr("Effects")
// 		columns: 1
// 		visible: orientation.value == "tie" && eventDirection.value == "undirected"

// 		VariablesForm
// 		{
// 			preferredHeight: 100 * preferencesModel.uiScale
// 			AvailableVariablesList 
// 			{ 
// 				name: "possibleEndogenousEffectsTieUndirected"; 
// 				title: qsTr("Endogenous effects")
// 				values:
// 				[
// 					{ label: qsTr("Degree difference"),							value: "degreeDiff"				},
// 					{ label: qsTr("Degree maximum"),								value: "degreeMax"				},
// 					{ label: qsTr("Degree Minimum"),								value: "degreeMin"				},
// 					{ label: qsTr("Fixed effects for event type"),	value: "FEtype"						},
// 					{ label: qsTr("Inertia"),												value: "inertia"					},
// 					{ label: qsTr("Pshift AB-AB"),									value: "psABAB"						},
// 					{ label: qsTr("Pshift AB-AY"),									value: "psABAY"						},
// 					{ label: qsTr("Shared partners"),								value: "sp"								},
// 					{ label: qsTr("Unique shared partners"),				value: "spUnique"					},
// 					{ label: qsTr("Recency continue"),							value: "recencyContinue"	},
// 					{ label: qsTr("Total degree dyad"),							value: "totaldegreeDyad"	},
// 					{ label: qsTr("User statistics"),								value: "userStat"					}
// 				]
// 			}

// 			AssignedVariablesList { name: "specifiedEndogenousEffectsTieUndirected"}

// 		}

// 		DropDown
// 		{
// 			id: exDropTieUndir
// 			name: "exogenousEffectsTieUndirected"
// 			label: qsTr("Exogenous effects")
// 			addEmptyValue: true
// 			values:
// 			[
// 				{ label: qsTr("Average"),			value: "averageEffect"		},
// 				{ label: qsTr("Difference"),	value: "differenceEffect"	},
// 				{ label: qsTr("Event"),				value: "eventEffect"			},
// 				{ label: qsTr("Maximum"),			value: "maximumEffect"		},
// 				{ label: qsTr("Minimum"),			value: "minimumEffect"		},
// 				{ label: qsTr("Same"),				value: "sameEffect"				},
// 				{ label: qsTr("Tie"),					value: "tieEffect"				}
// 			]
// 		}

// 		VariablesForm
// 		{
// 			preferredHeight: 200 * preferencesModel.uiScale
// 			AvailableVariablesList{	name:	"effectsVariablesListTieUndirected"; source: "allVariables"}
			
// 			AssignedVariablesList	
// 			{	
// 				name:	"averageEffectBoxTieUndirected"
// 				title: qsTr("Average effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["averageEffect"].includes(exDropTieUndir.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"differenceEffectBoxTieUndirected"
// 				title: qsTr("Difference effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["differenceEffect"].includes(exDropTieUndir.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"eventEffectBoxTieUndirected"
// 				title: qsTr("Event effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["eventEffect"].includes(exDropTieUndir.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"maximumEffectBoxTieUndirected"
// 				title: qsTr("Maximum effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["maximumEffect"].includes(exDropTieUndir.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"minimumEffectBoxTieUndirected"
// 				title: qsTr("Minimum effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["minimumEffect"].includes(exDropTieUndir.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"sameEffectBoxTieUndirected"
// 				title: qsTr("Same effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["sameEffect"].includes(exDropTieUndir.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"tieEffectBoxTieUndirected"
// 				title: qsTr("Tie effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["tieEffect"].includes(exDropTieUndir.value)
// 			}
// 		}

// 		// Group
// 		// {
// 		// 	title: qsTr("Interaction Effects")
// 		// 	VariablesForm
// 		// 	{
// 		// 		preferredHeight: 200 * preferencesModel.uiScale
// 		// 		AvailableVariablesList
// 		// 		{	
// 		// 			name:	"interactionEffectsList"
// 		// 			source: ["groupEffectVariable", "weekendEffectVariable", "settingEffectVariable", "differenceEffectVariables", "minimumEffectVariables", "maximumEffectVariables"] 
// 		// 		}
// 		// 		AssignedVariablesList	
// 		// 		{
// 		// 			name:	"interactions"
// 		// 			title: qsTr("Model interactions")
// 		// 			listViewType: JASP.Interaction
// 		// 			addAvailableVariablesToAssigned: false
// 		// 		}
// 		// 	}
// 		// }
// 	}


//  // section visible for the actor-oriented sender model
// 	Section 
// 	{
// 		title: qsTr("Effects Sender Model")
// 		columns: 1
// 		visible: orientation.value == "actorOriented"

// 		VariablesForm
// 		{
// 			preferredHeight: 100 * preferencesModel.uiScale
// 			AvailableVariablesList 
// 			{ 
// 				name: "possibleEndogenousEffectsActorSender"; 
// 				title: qsTr("Endogenous effects")
// 				values:
// 				[
// 					{ label: qsTr("In degree sender"),							value: "indegreeSender"					},
// 					{ label: qsTr("Out degree sender"),							value: "outdegreeSender"				},
// 					{ label: qsTr("Recency receive of sender"),			value: "recencyReceiveSender"		},
// 					{ label: qsTr("Recency send of sender"),				value: "recencySendSender"			},
// 					{ label: qsTr("Total degree sender"),						value: "totaldegreeSender"			},
// 					{ label: qsTr("User statistics"),								value: "userStat"								}
// 				]
// 			}

// 			AssignedVariablesList { name: "specifiedEndogenousEffectsActorSender"}

// 		}

// 		DropDown
// 		{
// 			id: exDropActorSender
// 			name: "exogenousEffectsActorSender"
// 			label: qsTr("Exogenous effects")
// 			addEmptyValue: true
// 			values:
// 			[
// 				{ label: qsTr("Send"),				value: "sendEffect"				}
// 			]
// 		}

// 		VariablesForm
// 		{
// 			preferredHeight: 200 * preferencesModel.uiScale
// 			AvailableVariablesList{	name:	"effectsVariablesListActorSender"; source: "allVariables"}
			
// 			AssignedVariablesList	
// 			{	
// 				name:	"sendEffectBoxActorSender"
// 				title: qsTr("Send effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["sendEffect"].includes(exDropActorSender.value)
// 			}
// 		}

// 		// Group
// 		// {
// 		// 	title: qsTr("Interaction Effects")
// 		// 	VariablesForm
// 		// 	{
// 		// 		preferredHeight: 200 * preferencesModel.uiScale
// 		// 		AvailableVariablesList
// 		// 		{	
// 		// 			name:	"interactionEffectsList"
// 		// 			source: ["groupEffectVariable", "weekendEffectVariable", "settingEffectVariable", "differenceEffectVariables", "minimumEffectVariables", "maximumEffectVariables"] 
// 		// 		}
// 		// 		AssignedVariablesList	
// 		// 		{
// 		// 			name:	"interactions"
// 		// 			title: qsTr("Model interactions")
// 		// 			listViewType: JASP.Interaction
// 		// 			addAvailableVariablesToAssigned: false
// 		// 		}
// 		// 	}
// 		// }
// 	}

//  // section visible for the actor-oriented receiver model
// 	Section 
// 	{
// 		title: qsTr("Effects Receiver Model")
// 		columns: 1
// 		visible: orientation.value == "actorOriented"

// 		VariablesForm
// 		{
// 			preferredHeight: 100 * preferencesModel.uiScale
// 			AvailableVariablesList 
// 			{ 
// 				name: "possibleEndogenousEffectsActorReceiver"; 
// 				title: qsTr("Endogenous effects")
// 				values:
// 				[
// 					{ label: qsTr("In degree receiver"),						value: "indegreeReceiver"				},
// 					{ label: qsTr("Inertia"),												value: "inertia"					},
// 					{ label: qsTr("Incoming shared partners"),			value: "isp"										},
// 					{ label: qsTr("Incoming two-path"),							value: "itp"										},
// 					{ label: qsTr("Outgoing shared partners"),			value: "osp"										},
// 					{ label: qsTr("Outgoing two-path"),							value: "otp"										},
// 					{ label: qsTr("Out deregee receiver"),					value: "outdegreeReceiver"			},
// 					{ label: qsTr("Recency continue"),							value: "recencyContinue"	},
// 					{ label: qsTr("Recency receive of receiver"), 	value: "recencyReceiveReceiver"	},
// 					{ label: qsTr("Recency send of receiver"),			value: "recencySendReceiver"		},
// 					{ label: qsTr("Reciprocity"),										value: "reciprocity"						},
// 					{ label: qsTr("Recency rank receive"),					value: "rrankReceive"						},
// 					{ label: qsTr("Recency rank send"),							value: "rrankSend"							},
// 					{ label: qsTr("Total degree receiver"),					value: "totaldegreeReceiver"		},
// 					{ label: qsTr("User statistics"),								value: "userStat"								}
// 				]
// 			}

// 			AssignedVariablesList { name: "specifiedEndogenousEffectsActorReceiver"}

// 		}

// 		DropDown
// 		{
// 			id: exDropActorReceiver
// 			name: "exogenousEffectsActorReceiver"
// 			label: qsTr("Exogenous effects")
// 			addEmptyValue: true
// 			values:
// 			[
// 				{ label: qsTr("Average"),			value: "averageEffect"		},
// 				{ label: qsTr("Difference"),	value: "differenceEffect"	},
// 				{ label: qsTr("Receive"),			value: "receiveEffect"		},
// 				{ label: qsTr("Same"),				value: "sameEffect"				},
// 				{ label: qsTr("Tie"),					value: "tieEffect"				}
// 			]
// 		}

// 		VariablesForm
// 		{
// 			preferredHeight: 200 * preferencesModel.uiScale
// 			AvailableVariablesList{	name:	"effectsVariablesListActorReceiver"; source: "allVariables"}
			
// 			AssignedVariablesList	
// 			{	
// 				name:	"averageEffectBoxActorReceiver"
// 				title: qsTr("Average effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["averageEffect"].includes(exDropActorReceiver.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"differenceEffectBoxActorReceiver"
// 				title: qsTr("Difference effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["differenceEffect"].includes(exDropActorReceiver.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"receiveEffectBoxActorReceiver"
// 				title: qsTr("Receive effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["receiveEffect"].includes(exDropActorReceiver.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"sameEffectBoxActorReceiver"
// 				title: qsTr("Same effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["sameEffect"].includes(exDropActorReceiver.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"sendEffectBoxActorReceiver"
// 				title: qsTr("Send effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["sendEffect"].includes(exDropActorReceiver.value)
// 			}
// 			AssignedVariablesList	
// 			{	
// 				name:	"tieEffectBoxActorReceiver"
// 				title: qsTr("Tie effect")
// 				suggestedColumns: ["nominal"]
// 				singleVariable: false
// 				visible: ["tieEffect"].includes(exDropActorReceiver.value)
// 			}
// 		}

// 		// Group
// 		// {
// 		// 	title: qsTr("Interaction Effects")
// 		// 	VariablesForm
// 		// 	{
// 		// 		preferredHeight: 200 * preferencesModel.uiScale
// 		// 		AvailableVariablesList
// 		// 		{	
// 		// 			name:	"interactionEffectsList"
// 		// 			source: ["groupEffectVariable", "weekendEffectVariable", "settingEffectVariable", "differenceEffectVariables", "minimumEffectVariables", "maximumEffectVariables"] 
// 		// 		}
// 		// 		AssignedVariablesList	
// 		// 		{
// 		// 			name:	"interactions"
// 		// 			title: qsTr("Model interactions")
// 		// 			listViewType: JASP.Interaction
// 		// 			addAvailableVariablesToAssigned: false
// 		// 		}
// 		// 	}
// 		// }
// 	}

	Section
	{
		title: qsTr("Estimation Options")

		RadioButtonGroup {
			title: qsTr("Estimation method")
			name: "method"
			RadioButton { value: "MLE" ; label: qsTr("Maximum likelihood estimation"); checked: true}
			RadioButton { value: "GDADAMAX" ; label: qsTr("Adaptive gradient descent"); checked: false}
		}
	}

		Section
	{
		title: qsTr("Advanced Options")

		RadioButtonGroup
		{
			title: 	qsTr("Missing Values")
			name: 	"naAction"

			RadioButton { value: "pairwise"; label: qsTr("Exclude cases pairwise"); checked: false}
			RadioButton { value: "listwise"; label: qsTr("Exclude cases listwise"); checked: true}
		}
	}

}






				// DropDown
		// {
		// 	id: exDropTieDir
		// 	name: "exogenousEffectsTieDirected"
		// 	label: qsTr("Exogenous effects")
		// 	addEmptyValue: true
		// 	values:
		// 	[
		// 		{ label: qsTr("Average"),			value: "averageEffect"		},
		// 		{ label: qsTr("Difference"),	value: "differenceEffect"	},
		// 		{ label: qsTr("Event"),				value: "eventEffect"			},
		// 		{ label: qsTr("Maximum"),			value: "maximumEffect"		},
		// 		{ label: qsTr("Minimum"),			value: "minimumEffect"		},
		// 		{ label: qsTr("Receive"),			value: "receiveEffect"		},
		// 		{ label: qsTr("Same"),				value: "sameEffect"				},
		// 		{ label: qsTr("Send"),				value: "sendEffect"				},
		// 		{ label: qsTr("Tie"),					value: "tieEffect"				}
		// 	]
		// }

		// VariablesForm
		// {
		// 	preferredHeight: 200 * preferencesModel.uiScale
		// 	AvailableVariablesList{	name:	"effectsVariablesListTieDirected"; source: "allVariables"}
			
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"averageEffectBoxTieDirected"
		// 		title: qsTr("Average effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["averageEffect"].includes(exDropTieDir.value)
		// 	}
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"differenceEffectBoxTieDirected"
		// 		title: qsTr("Difference effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["differenceEffect"].includes(exDropTieDir.value)
		// 	}
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"eventEffectBoxTieDirected"
		// 		title: qsTr("Event effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["eventEffect"].includes(exDropTieDir.value)
		// 	}
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"maximumEffectBoxTieDirected"
		// 		title: qsTr("Maximum effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["maximumEffect"].includes(exDropTieDir.value)
		// 	}
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"minimumEffectBoxTieDirected"
		// 		title: qsTr("Minimum effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["minimumEffect"].includes(exDropTieDir.value)
		// 	}
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"receiveEffectBoxTieDirected"
		// 		title: qsTr("Receive effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["receiveEffect"].includes(exDropTieDir.value)
		// 	}
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"sameEffectBoxTieDirected"
		// 		title: qsTr("Same effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["sameEffect"].includes(exDropTieDir.value)
		// 	}
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"sendEffectBoxTieDirected"
		// 		title: qsTr("Send effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["sendEffect"].includes(exDropTieDir.value)
		// 	}
		// 	AssignedVariablesList	
		// 	{	
		// 		name:	"tieEffectBoxTieDirected"
		// 		title: qsTr("Tie effect")
		// 		suggestedColumns: ["nominal"]
		// 		singleVariable: false
		// 		visible: ["tieEffect"].includes(exDropTieDir.value)
		// 	}
		// }