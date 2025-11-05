import QtQuick 2.12
import QtQuick.Layouts 1.12
import JASP 1.0

Form {
    title: qsTr("Synthetic Data")

    Analysis {
        id: analysis
        // Must match the R entrypoint function name
        name: "syntheticData"
        title: qsTr("Synthetic Data")
    }

    Section {
        title: qsTr("Variables")
        VariablesList {
            id: vars
            name: "variables"        // -> options$variables in R
            title: qsTr("Select variables")
            // Keep simple: allow any column types. (If you want to constrain, we can add that later.)
        }
    }

    Section {
        title: qsTr("Generation options")
        ColumnLayout {
            spacing: 8

            IntegerField {
                name: "n"            // -> options$n
                title: qsTr("Rows to synthesize (n)")
                defaultValue: 0      // 0 = same as original n (handled in R)
                min: 0
                info: qsTr("Set 0 to match the size of the input data.")
            }

            IntegerField {
                name: "seed"         // -> options$seed
                title: qsTr("Random seed")
                defaultValue: 123
                min: 0
            }
        }
    }

    // No Results section needed here — R builds tables/plots via jaspBase.
}
