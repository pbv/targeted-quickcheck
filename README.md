
# Targeted-QuickCheck

Targeted generators for QuickCheck, based on the ISSTA'17 paper
"Targeted Property-Based Testing" by Andreas Löscher and
Konstantino Sogonas.

The key idea is to use an objetive function and classical search
algorithms to guide the generation of inputs for properties.  This
should allow for more through testing of properties when the
relevant cases are sparse over the state space of the inputs.

This library implements generation using Hill Climbing and Simulated
Annealing. To use it, simply import `Test.QuickCheck.Target`
(alongside the usual QuickCheck imports) and write generators using
the strategies defined in this module.  See the examples module for
guidance.

----

Pedro Vasconcelos, 2025.
