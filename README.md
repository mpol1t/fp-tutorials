# Functional Programming Tutorial Solutions

This repository contains solutions to the tutorial exercises for the **Informatics 1 – Functional Programming** module at the **University of Edinburgh**. The exercises cover fundamental topics in Haskell programming, including list comprehensions, recursion, higher-order functions, logic programming, and more.

## Structure

Each tutorial has a corresponding Haskell file containing solutions to the exercises. The tutorials are organized as follows:

- **Tutorial 1** – List Comprehensions
- **Tutorial 2** – Recursion & the Caesar Cipher
- **Tutorial 3** – Higher-Order Functions
- **Tutorial 4** – Screen Scraping
- **Tutorial 5** – Logic and Propositional Calculus
- **Tutorial 6** – Turtle Graphics and L-systems
- **Tutorial 7** – Barcode Reader
- **Tutorial 8** – Finite State Machines

## Getting Started

1. Clone the repository:
   ```bash
   git clone https://github.com/mpol1t/fp-tutorials.git
   cd fp-tutorials
   ```
2. Load the Haskell files into GHCi:
   ```bash
   ghci tutorial1.hs
   ```
3. Run functions and test cases as needed.

## Dependencies

Some tutorials may require additional Haskell packages. You can install them using:
```bash
cabal update
cabal install <package-name>
```

### Installing `GLUT` for Tutorial 6 (Turtle Graphics)

If you're on **Debian-based systems (Ubuntu, etc.)**, run the following:

```bash
apt update && apt install freeglut3-dev -y
cabal update
cabal install --lib GLUT
```

To make life easier, create a `.ghci` file in your project directory with the following contents:

```
:set -package OpenGL
:set -package random
:l tutorial6.hs
```

## Notes

- These solutions are for educational purposes and should be used as a reference.
- Students are encouraged to attempt the exercises independently before consulting these solutions.
- Some exercises require **QuickCheck** for property-based testing. Install it using:
  ```bash
  cabal install QuickCheck
  ```
  Then, you can run tests in GHCi:
  ```haskell
  quickCheck prop_someFunction
  ```

## License

This repository is provided for educational purposes. Please refer to your university's academic integrity policies before using these solutions.
