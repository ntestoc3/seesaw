#!/bin/env sh

lein pom

lein localrepo install target/seesaw-0.1.8.jar ntestoc/seesaw 0.1.8 -p pom.xml
