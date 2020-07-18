find test/ -name '*_expected' -exec sed -i -e 's/<->/↔/g' -e 's/->/→/g' -e 's/|-/⊢/g' -e 's/\&/∧/g' -e 's/!/∃/g' -e 's/|/∨/g' -e 's/@/∀/g' -e 's/!/∃/g' {} \;
