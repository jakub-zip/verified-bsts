# verified-bsts
Verified implementations of balanced binary search trees. Contains implementations of LLRB and AVL trees. Uses the Scala programming language and the Stainless verifier. This code was written during the creation of my bachelor's thesis.

# Verification

To build the container, run:
```
docker build -t bachelor-thesis-code .
```

After building, run the container using:
```
docker run --rm bachelor-thesis-code
```

The entire verification should take about ~10 minutes.

After the verification finishes, you can delete the container using
```
docker rmi bachelor-thesis-code
```

# Ready-for-use implementation tests

To run the tests use
```
sbt test
```

This software was developed with the support of the Faculty of Information Technology, Czech Technical University in Prague, fit.cvut.cz
![FIT ČVUT logo](assets/logo-fit-en-cerna.png)