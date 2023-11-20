### Instructions for Challenge 3 - Statistical Modelling

Time to delve into statistical modelling to answer a research question in our data science course. 

In the statistical modelling challenge, you are individual consultants hired by the WWF to put together a report on the population trends of a species from the Living Planet Database (http://www.livingplanetindex.org/home/index).  

The models assignment involves four components:

1. Choosing a species and indicating which species you choose in the issue thread. Each student must choose a **DIFFERENT** species (communicate in GitHub to make sure you have all chosen different species).

2. Design the research question(s), hypotheses and predictions to test the change in abundance over time for your species. Fill out a study pre-registration and push it to your repository **BEFORE** you begin working with the data.

3. Build and interpret a mixed effects model in **EITHER** a frequentist or Bayesian framework (all the work associated with it - code, graphs, interpretation, see below for the specific marking criteria).

4. Provide a brief summary report for all of your work including question, hypotheses, predictions, statistical model, statistical summaries, figures of the data and the statistical models fit including predictions of the model and error and summarise all content in one markdown file in your challenge repository.

**REMEMBER:** There are no "right" or "wrong" ways to statistically model data, there are just different decisions that you make that you can justify to different extents. We want to see your thinking on this challenge, so justify all of the decisions that you make!

__Marking criteria:__

- __Challenge: Clear evidence of work on a hierarchical statistical model. Well-formatted and easy to interpret tables of model outputs for the model. Appropriate model visualisations for the model. (25%).__ Your repository should include report that contains all your work on the challenge. It should be a `Markdown` file that includes all your code for your hierarchical model, text to explain your modelling decisions, the graphs and tables. The table(s) should be generated with code. Think about what's an appropriate number of digits to show, specifying what type of variables are included in the models, etc. Think particularly about spatial replication and temporal replication in your model when choosing your random effects. There are a few different `R` packages out there that help you visualise model summaries, so you should look into those and not make the tables or figures "by hand". The tables should not be "static", e.g., the numbers should not be hard coded, but should update every time the model is run. Particularly, if you choose to model in a Bayesian framework, every time you rerun a Bayesian model, the results will be slightly different and you should use the model `R` object to generate the table automatically, not e.g., making an object called `table` and assigning `table <- as.data.frame(c(0.005, 0.023, 0.567, etc.))`. Your `Markdown` document should include at least one graph of your model, but you may want to include more - the graphs should include the raw data points and the model predictions including the error. Make sure you know what the model predictions are and how to plot them - there is a lot of information on plotting model predictions online and in the Coding Club tutorials, so check that out. You can also look into visualising the fixed and random effects of the hierarchical models, again explore online the different packages for visualising mixed effects models (as discussed in class).

- __Creativity: Creative content and formatting of your challenge elements (25%).__ Demonstrate creative data visualisation and use of the data to communicate the trends in your species. Include additional content such as photographs and nice formatting of figures, tables and text and creative formatting to communicate to your audience the WWF. Creativity and efficiency in the code is also encouraged. But, remember that the report should be short, clear and easy for the WWF to skim and quickly understand - less is more!

- __Reproducibility: Clear, logical and critical explanation of your workflows, statistical reporting and results (25%).__ Your repository and final summary report (markdown file) should include the research question, the hypotheses, predictions and explanations of the types of variables you'll test. You are encouraged to include a summary of existing information about the species and a few references in your report (up to five or so). Use your ecological expertise and outline why you are making your specific predictions. You should demonstrate critical thinking at all stages of your work - for example include information on why you chose the structure of model that you did. No model is perfect, there are always compromises, and we want to see evidence of critical thinking with the model design. You should include a pre-registration document that you fill out before working with the data (see markdown template file in the preregistration folder).

- __Participation: Evidence of consistent work on the challenge, issue participation, discussion of problems, providing solutions, sharing useful information (25%).__ - The idea behind this challenge is to work on the assignment consistently throughout building on what you are learning in the different coding club tutorials. Like with previous challenges, communicate with your peers from the beginning and throughout the challenge, don't wait till a few days before the deadline to make the issue and participate in it - we want to see you discussing your work consistently through out the challenge with your peers, which will help to make everyone's work better.

#### Bonuses

__If you are keen, we have a couple of non-compulsory elements to this challenge that can be a way for you to elevate your marks on the above assessment criteria.__

- Describe in your `Markdown` document why (or why not if you decide to argue a different path) a Bayesian hierarchical model would be the best approach to use.

- Include any or all of the code, results, summary model outputs, model visualisations and interpretation for a Bayesian hierarchical model.

__For an extra bonus, you can provide a functioning model (hierarchical or not) written in the modelling language Stan!!!__

As with all challenges, we expect nicely organised repositories with appropriate folders, files and structure.

__Useful links:__
- https://ourcodingclub.github.io/tutorials/modelling/ - Intro to linear models
- https://ourcodingclub.github.io/tutorials/model-design/ - Intro to model design
- https://ourcodingclub.github.io/tutorials/mixed-models/ - Intro to mixed effects models
- https://ourcodingclub.github.io/tutorials/data-vis-2/ - Data visualisation (including plotting mixed effects)
- https://ourcodingclub.github.io/tutorials/mcmcglmm/ - Intro to Bayesian statistics
- https://ourcodingclub.github.io/tutorials/stan-intro/ - Intro to Stan
- https://ourcodingclub.github.io/tutorials/stan-2/ - Generalised linear models in Stan
- https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf - lme4 vignette
- http://cran.nexr.com/web/packages/MCMCglmm/vignettes/CourseNotes.pdf - MCMCglmm course notes
- https://github.com/paul-buerkner/brms - brms package resources (scroll down to the README.md file)
- https://mc-stan.org/ - the Stan website
