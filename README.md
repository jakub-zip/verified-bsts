# verified-bsts
Verified implementations of balanced binary search trees. Contains implementations of LLRB and AVL trees. Uses the Scala programming language and the Stainless verifier. This code was written during the creation of my bachelor's thesis.

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

This software was developed with the support of the Faculty of Information Technology, Czech Technical University in Prague, fit.cvut.cz
![FIT ČVUT logo](assets/logo-fit-en-cerna.png)