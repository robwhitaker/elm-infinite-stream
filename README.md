# Elm Infinite Stream

A library for creating lazy, infinite streams. Good for random generators, number sequences, and anything else that needs to be generated indefinitely.

**Warning:** While using this library, keep in mind that streams are _infinite_, so if a function never matches a terminating condition, that function will run forever and cause your program to hang. All functions where this is possible have been marked with a warning.