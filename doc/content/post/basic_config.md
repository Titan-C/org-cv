+++
title = "Basic Org file"
author = ["Óscar Nájera"]
draft = false
weight = 1003
+++

The basic structure of an org file containing your CV is shown next.


## Personal contact information {#personal-contact-information}

`TITLE`, `AUTHOR` and `EMAIL` are standard org options. But on `TITLE` you
put your foreseen job.

<div class="ox-hugo-table table table-striped">
<div></div>

| Field    | Description                                        |
|----------|----------------------------------------------------|
| TITLE    | Desired job                                        |
| AUTHOR   | Who you are?                                       |
| EMAIL    | Your contact email                                 |
| ADDRESS  | Mailing address, this can span over multiple lines |
| HOMEPAGE | URL of your website                                |
| MOBILE   | Mobile phone                                       |
| GITHUB   | GitHub user                                        |
| GITLAB   | GitLab user                                        |
| LINKEDIN | Linkedin username                                  |
| PHOTO    | path to photo file                                 |

</div>

```org
#+TITLE: My dream job
#+AUTHOR: John Doe
#+email: john@doe.lost

#+ADDRESS: My Awesome crib
#+ADDRESS: Fantastic city -- Planet Earth
#+MOBILE: (+9) 87654321
#+HOMEPAGE: example.com
#+GITHUB: Titan-C
#+GITLAB: Titan-C
#+LINKEDIN: oscar-najera
#+PHOTO: smile.png
```

You can use org-modes hierarchical structure to describe your CV. To make a
specific subtree an item describing an experience point (Job you have,
degree you pursued, etc.) you use the org properties drawer and with the
`:CV_ENV: cventry` property. You should also include the `FROM` and `TO`
properties defining the span of the event, as `LOCATION` and `EMPLOYER`.

```org
* Employement
** One job
:PROPERTIES:
:CV_ENV: cventry
:FROM:     <2014-09-01>
:TO:     <2017-12-07>
:LOCATION: a city, a country
:EMPLOYER: The employer
:END:

I write about awesome stuff I do.
** Other job
:PROPERTIES:
:CV_ENV: cventry
:FROM:     <2013-09-01>
:TO:     <2014-08-07>
:LOCATION: my city, your country
:EMPLOYER: The other employer
:END:

I write about awesome stuff I do.

* Other stuff I do
- I work a lot
- I sleep a lot
- I eat a lot
```
