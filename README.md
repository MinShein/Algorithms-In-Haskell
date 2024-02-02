<!-- Static Typing -->
<h3>1. Static Typing:</h3>
<pre>
<code>
-- Type checked at compile-time
add :: Int -> Int -> Int
add x y = x + y
</code>
</pre>

<!-- Dynamic Typing -->
<h3>2. Dynamic Typing:</h3>
<pre>
<code>
-- Types checked at runtime
dynamicAdd :: a -> a -> a
dynamicAdd x y = x  -- This is not an error until runtime
</code>
</pre>

<!-- Strong Typing -->
<h3>3. Strong Typing:</h3>
<pre>
<code>
-- Prohibits operations between incompatible types
-- Error: Can't add an Int to a String
-- result = 10 + "5"
</code>
</pre>

<!-- Weak Typing -->
<h3>4. Weak Typing:</h3>
<pre>
<code>
-- Allows operations without explicit type conversion
weakAdd :: Int -> String -> String
weakAdd x y = show x ++ y
</code>
</pre>

<!-- Nominal Typing -->
<h3>5. Nominal Typing:</h3>
<pre>
<code>
-- Explicit type declarations by name
data Animal = Dog | Cat

speak :: Animal -> String
speak Dog = "Woof!"
speak Cat = "Meow!"
</code>
</pre>

<!-- Structural Typing -->
<h3>6. Structural Typing:</h3>
<pre>
<code>
-- Types based on structure
data Point = Point { x :: Int, y :: Int }

distance :: Point -> Int
distance p = abs (x p) + abs (y p)
</code>
</pre>

<!-- Dependent Typing -->
<h3>7. Dependent Typing:</h3>
<pre>
<code>
-- Types depend on values
-- A simple example in Idris
totalLength : (s : String) -> Nat
totalLength str = length str
</code>
</pre>

<!-- Parametric Polymorphism -->
<h3>8. Parametric Polymorphism:</h3>
<pre>
<code>
-- Generic function and type
identity :: a -> a
identity x = x

data Pair a b = Pair a b
</code>
</pre>

<!-- Ad-hoc Polymorphism -->
<h3>9. Ad-hoc Polymorphism:</h3>
<pre>
<code>
-- Example of type classes in Haskell
class Printable a where
    printIt :: a -> String

instance Printable Int where
    printIt x = show x

instance Printable Bool where
    printIt True = "True"
    printIt False = "False"
</code>
</pre>
