import QtQuick 
import QtQuick.Layouts
import QtQuick.Controls
import JASP
import JASP.Controls

Form {
    title: qsTr("Synthetic Data")
    info: qsTr("Configure variables and options to generate a synthetic dataset.")
    property string lastSavePath: (typeof analysisState !== "undefined" && analysisState.lastSavePath) ? analysisState.lastSavePath : ""

    VariablesForm {
        AvailableVariablesList { name: "allVariables" }

        AssignedVariablesList {
            name: "variables"    // -> options$variables in R
            label: qsTr("Variables")
            info: qsTr("Columns copied here will be simulated.")
        }
    }

    Section {
        title: qsTr("Options")
        ColumnLayout {
            spacing: 8

            RadioButtonGroup {
                name: "rowCountMode"
                title: qsTr("Row count")
                info: qsTr("Pick whether to keep the same row count as the input data or supply a new total.")

                RadioButton { id: sameRowCountRadio; value: "same"; label: qsTr("Same number of rows as data"); checked: true }
                RadioButton { id: customRowCountRadio; value: "custom"; label: qsTr("Change number of rows") }
            }

            IntegerField {
                visible: customRowCountRadio.checked
                Layout.minimumHeight: customRowCountRadio.checked ? implicitHeight : 0
                Layout.maximumHeight: customRowCountRadio.checked ? implicitHeight : 0
                Layout.preferredHeight: customRowCountRadio.checked ? implicitHeight : 0
                name: "n"            // -> options$n
                label: qsTr("Number of rows")
                defaultValue: 100
                min: 1
                info: qsTr("Set the exact number of rows to generate when you choose to change the dataset size.")
                fieldWidth: 100 * jaspTheme.uiScale
            }

            IntegerField {
                id: seedField
                name: "seed"         // -> options$seed
                label: qsTr("Random seed")
                defaultValue: Math.floor(Math.random() * 1000000)
                min: 0
                info: qsTr("Change this number to control reproducibility.")
                fieldWidth: 100 * jaspTheme.uiScale
            }

            DoubleField {
                name: "jitterFraction"     // -> options$jitterFraction
                label: qsTr("Jitter fraction")
                defaultValue: 0.05
                min: 0
                max: 1
                info: qsTr("Controls the size of jitter applied to numeric columns after resampling rows. Zero means no jitter.")
                fieldWidth: 100 * jaspTheme.uiScale
            }
        }
    }

    Section {
        title: qsTr("Save synthetic dataset")
        ColumnLayout {
            spacing: 8

            FileSelector {
                name: "fileFull"
                label: qsTr("Save as…")
                placeholderText: qsTr("synthetic-data.csv")
                filter: "*.csv"
                save: true
                value: lastSavePath
                Layout.fillWidth: true
                info: qsTr("Pick a file path (you can type it or browse once) and then click Run/Enter again to write the CSV to disk—re-entering the path isn’t necessary. You can save by clicking in the box and hitting the enter key.")
            }
        }
    }

    // No Results section needed here — R builds tables/plots via jaspBase.
}
