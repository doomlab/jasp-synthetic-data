import QtQuick 
import QtQuick.Layouts
import QtQuick.Controls
import JASP
import JASP.Controls

Form {
    title: qsTr("Synthetic Data")
    info: qsTr("Configure variables and options to generate a synthetic dataset.")

    property string downloadUrl: (typeof analysisState !== "undefined" && analysisState.downloadUrl) ? analysisState.downloadUrl : ""

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

            IntegerField {
                name: "n"            // -> options$n
                label: qsTr("Number of rows")
                defaultValue: 0      // 0 = same as original n (handled in R)
                min: 0
                info: qsTr("Set 0 to match the size of the input data.")
            }

            IntegerField {
                id: seedField
                name: "seed"         // -> options$seed
                label: qsTr("Random seed")
                defaultValue: Math.floor(Math.random() * 1000000)
                min: 0
                info: qsTr("Change this number to control reproducibility.")
            }
        }
    }

    Section {
        title: qsTr("Download")
        ColumnLayout {
            spacing: 8

            Text {
                text: downloadUrl !== "" ? qsTr("Click the button to download the latest synthetic dataset.") : qsTr("Run the analysis to enable download.")
            }

            Button {
                text: qsTr("Download synthetic data")
                enabled: downloadUrl !== ""
                onClicked: {
                    if (downloadUrl !== "") {
                        Qt.openUrlExternally(downloadUrl)
                    }
                }
            }
        }
    }

    // No Results section needed here — R builds tables/plots via jaspBase.
}
