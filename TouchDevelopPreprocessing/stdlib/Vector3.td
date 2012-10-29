
// Gets the length of the vector
length : Number
// Linear interpolation between two vectors
linear interpolation(other : Vector3, amount : Number) : Vector3

// Turns the vector into a string
var to_string : String

// Gets the x-component
var x : Number

// Gets the y-component
var y : Number

// Gets the z-component
var z : Number

// Adds a vector
add(other : Vector3) : Vector3

// Subtracts another vector
subtract(other : Vector3) : Vector3

// Multiplies with a scaling factor
scale(scalar : Number) : Vector3

// Returns a vector of one unit pointing in the same direction as the original vector
normalize : Vector3

// Returns a vector pointing in the opposite direction
negate : Vector3

// Multiplies component-wise with a vector
multiply(other : Vector3) : Vector3

// Restricts the vector in the specified range
clamp(min : Vector3, max : Vector3) : Vector3

// Calculates the cross product with the other vector
cross(other : Vector3) : Vector3

// Gets the distance between the two vectors
distance(other : Vector3) : Number