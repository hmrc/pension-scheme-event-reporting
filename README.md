# Pension Scheme Event Reporting



## Technical documentation

### Before running the app (if applicable)

Anything that's not done automatically by the development environment:

- You need to run mongo
- ```sm2 --start PODS_ER```

### Running the test suite

```
sbt clean
sbt compile
sbt test
```

### Further documentation

A list of links to key files in docs/.

You can also just link to the docs/ directory itself.

- [Overview](#overview)
- [Requirements](#requirements)
- [Running the Service](#running-the-service)
- [Enrolments](#enrolments)
- [Compile & Test](#compile--test)
- [Dependent Services](#dependent-services)
- [Service Documentation](#service-documentation)
- [Endpoints Used](#endpoints)
- [License](#license)

## Overview

This is the backend repository for the *TEMPLATE* service. This service allows a user to *DO X, Y AND Z. Clarify any other points overviewing the service here*.

This service has a corresponding front-end microservice, namely *NAME FRONTEND SERVICE*.

**Associated Frontend Link:** *INSERT LINK TO FRONTEND HERE*

**Stubs:** https://github.com/hmrc/pensions-scheme-stubs



## Requirements
This service is written in Scala and Play, so needs at least a [JRE] to run.

*VERIFY VERSIONS. VERSIONS CORRECT AS OF 20/11/2024*

**Node version:** 16.20.2

**Java version:** 11

**Scala version:** 2.13.14


## Running the Service
*VERIFY DETAILS. VERSIONS CORRECT AS OF 20/11/2024*
**Service Manager Profile:** PODS_ALL

**Port:** XXXX

**Link:** *http://localhost:XXXX/INSERT-BASE-URL-HERE*


In order to run the service, ensure Service Manager is installed (see [MDTP guidance](https://docs.tax.service.gov.uk/mdtp-handbook/documentation/developer-set-up/set-up-service-manager.html) if needed) and launch the relevant configuration by typing into the terminal:
`sm2 --start PODS_ALL`

To run the service locally, enter `sm2 --stop *REPONAMEHERE*`.

In your terminal, navigate to the relevant directory and enter `sbt run`.

Access the Authority Wizard and login with the relevant enrolment details [here](http://localhost:9949/auth-login-stub/gg-sign-in)


## Enrolments
There are several different options for enrolling through the auth login stub. In order to enrol as a dummy user to access the platform for local development and testing purposes, the following details must be entered on the auth login page.


In order to access the **Pension Practitioner dashboard** for local development, enter the following information: 

**Redirect URL -** http://localhost:8204/manage-pension-schemes/dashboard 

**GNAP Token -** NO 

**Affinity Group -** Organisation 

**Enrolment Key -** HMRC-PODSPP-ORG 

**Identifier Name -** PspID 

**Identifier Value -** 21000005

---

For access to the **Pension Administrator dashboard** for local development, enter the following information: 

**Redirect url -** http://localhost:8204/manage-pension-schemes/overview 

**GNAP Token -** NO 

**Affinity Group -** Organisation 

**Enrolment Key -** HMRC-PODS-ORG 

**Identifier Name -** PsaID 

**Identifier Value -** A2100005

---

**Dual enrolment** as both a Pension Administrator and Practitioner is also possible and can be accessed by entering:

**Redirect url -** http://localhost:8204/manage-pension-schemes/overview 

**GNAP Token -** NO 

**Affinity Group -** Organisation 

**Enrolment Key 1 -** HMRC-PODSPP-ORG Identifier

**Name 1 -** PspID Identifier

**Value 1 -** 21000005

**Enrolment Key 2 -** HMRC-PODS-ORG 

**Identifier Name 2 -** PsaID 

**Identifier Value 2 -** A2100005

---

To access the **Scheme Registration journey**, enter the following information:

**Redirect URL -** http://localhost:8204/manage-pension-schemes/you-need-to-register

**GNAP Token -** NO 

**Affinity Group -** Organisation

---


## Compile & Test
**To compile:** Run `sbt compile`

**To test:** Use `sbt test`

**To view test results with coverage:** Run `sbt clean coverage test coverageReport`

For further information on the PODS Test Approach and wider testing including acceptance, accessibility, performance, security and E2E testing, visit the PODS Confluence page [here](https://confluence.tools.tax.service.gov.uk/pages/viewpage.action?spaceKey=PODSP&title=PODS+Test+Approach).

For Journey Tests, visit the [Journey Test Repository](| Journey tests(https://github.com/hmrc/pods-journey-tests).

View the prototype [here](https://pods-event-reporting-prototype.herokuapp.com/).


## Dependent Services
There are numerous APIs implemented throughout the MPS architecture, and the relevant endpoints are illustrated below. For an overview of all PODS APIs, refer to the [PODS API Documentation](https://confluence.tools.tax.service.gov.uk/display/PODSP/PODS+API+Latest+Version).


## Service Documentation
[To Do]
Include relevant links or details to any additional, service-specific documents (e.g., stubs, testing protocols) when available.


## Endpoints
[To Do]

*Must add: standard docs, path, any args, expected request and a sample response and error codes and/or responses.*

**Standard Path**


**Description**


| *Args*                        | *Expected Requests*                      | *Samples Response*                           | *Error Codes/Responses*                   |
|-------------------------------|------------------------------------------|----------------------------------------------|-------------------------------------------|
| ```INSERT ARGS```             | INSERT REQUEST HERE                      | INSERT RESPONSE HERE                         | INSERT ERROR CODES AND RESPONSES          |


## License
This code is open source software Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at:

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

[↥ Back to Top](#pension-scheme-event-reporting)
