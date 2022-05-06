
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"method.var","title":"Estimator for the RE model","type":"List","options":[{"title":"REML","name":"REML"},{"title":"DL","name":"DL"},{"title":"hksj","name":"hksj"},{"title":"ML","name":"ML"},{"title":"PM","name":"PM"}],"default":"REML"},{"name":"true_effect","title":"Best approximation of the 'true' effect","type":"List","options":[{"title":"largest","name":"largest"},{"title":"pooled","name":"pooled"}],"default":"largest"},{"name":"mult.level","title":"Check the box if at least one factor has a multilevel structure","type":"Bool","default":false},{"name":"r","type":"Number","title":"Strength of the within-study correlation between the outcomes","min":0,"max":1,"default":0.5}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Calculations for an umbrella review",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.ComboBox,
					typeName: 'ComboBox',
					name: "method.var"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.ComboBox,
					typeName: 'ComboBox',
					name: "true_effect"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "mult.level"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "r",
					format: FormatDef.number
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
