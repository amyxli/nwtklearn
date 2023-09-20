
jsPsych.plugins["info-cue"] = (function() {

  var plugin = {};

  plugin.info = {
    name: "info-cue",
    description: "Simple 2-armed bandit task",
    parameters: {
      stimulus1: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: "Option 1",
        default_value: undefined,
        description: "Image file for Option 1."
      },
      stimulus2: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: "Option 2",
        default_value: undefined,
        description: "Image file for Option 2."
      },
      cue1: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: "Option 1 cue",
        default_value: undefined,
        description: "Possible outcomes (number of points) for selecting Option 1. Only used if Outcome Distribution == 'discrete'."
      },
      cue2: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: "Option 2 cue",
        default_value: undefined,
        description: "Possible outcomes (number of points) for selecting Option 2.Only used if Outcome Distribution == 'discrete'."
      },
      prob1: {
        type: jsPsych.plugins.parameterType.FLOAT,
        pretty_name: "Option 1 Cue Probabilities",
        default_value: null,
        description: "Probabilities associated with Option 1 outcomes. Must be an array of the same length as outcomes1. Only used if Outcome Distribution == 'discrete'."
      },
      prob2: {
        type: jsPsych.plugins.parameterType.FLOAT,
        pretty_name: "Option 2 Cue Probabilities",
        default_value: null,
        description: "Probabilities associated with Option 2 outcomes. Must be an array of the same length as outcomes2. Only used if Outcome Distribution == 'discrete'."
      },

      feedbackDuration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Feedback Duration",
        default_value: 1500,
        description: "How long (ms) to display feedback (reward)."
      },
      preTrialInterval: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Pre-trial Interval",
        default_value: 0,
        description: "How long (ms) before presenting choice stimuli."
      }
    }
  }

// -----------------------------------------------------------------------------------------------------------------

  plugin.trial = function(display_element, trial) {

    let stimulus1 = null;
    let stimulus2 = null;

    // Variable to store trial data
    var response = {
      choice: null,
      feedback: null,
      rt: null
    };

    // Update trial number
    trialNumber++;

    // Add content to css class in order to apply bandit styles
    const content = document.querySelector("#jspsych-content");
    content.classList.add("bandit-grid-container");

    // Set up CSS grid container in HTML
    display_element.innerHTML = `
        <div class="grid-item tally">Total Points: ${totalPoints}</div>
        <div id="option1" class="grid-item options"></div>
        <div id="option2" class="grid-item options"></div>
        <div class="grid-item next"></div>`;

    // Make variables for the options and next button
    var option1 = display_element.querySelector("#option1");
    var option2 = display_element.querySelector("#option2");
    var next = display_element.querySelector(".next");
    var outcome = null;

    // If probabilities parameters == "equal", fill array to use in sampling function
    if (trial.prob1 == "equal"){
      trial.prob1 = Array(trial.outcomes1.length).fill(1);
    }
    if (trial.prob2 == "equal"){
      trial.prob2 = Array(trial.outcomes2.length).fill(1);
    }

    // Display choice stimuli once pre-trial interval has elapsed
    if(trial.preTrialInterval == 0){
      displayChoice();
    } else {
      jsPsych.pluginAPI.setTimeout(function(){
        displayChoice();
      }, trial.preTrialInterval);
    }

// FUNCTIONS -------------------------------------------------------------------------------------------------------

    function displayChoice() {

      // Draw stimuli using HTML
      option1.innerHTML = `<img id="stimulus1" class="stimuli" src="${trial.stimulus1}" draggable="false">`;
      option2.innerHTML = `<img id="stimulus2" class="stimuli" src="${trial.stimulus2}" draggable="false">`;

      // Select the stimuli so you can interact with them
      stimulus1 = display_element.querySelector("#stimulus1");
      stimulus2 = display_element.querySelector("#stimulus2");

      // Start "listening" for the participant clicking the stimulus
      // When they click on it, run the function chooseOption
      stimulus1.addEventListener("click", chooseOption1);
      stimulus2.addEventListener("click", chooseOption2);

    };

    // Functions for when the participant chooses each option
    // Necessary because you can't pass parameters directly into addEventListener (above)
    function chooseOption1() {makeChoice(selection=1)}
    function chooseOption2() {makeChoice(selection=2)}

    // What happens when the participant clicks the stimulus
    function makeChoice(selection) {

      // Calculate reaction time
      const endTime = (new Date()).getTime();
      response_time = endTime - startTime;

      // Stop listening for clicks (so people can't select more than one option)
      stimulus1.removeEventListener("click", chooseOption1);
      stimulus2.removeEventListener("click", chooseOption2);

      // Calculate and display outcome of choice
      outcome = outcomeDiscrete(selection);
      feedbackCue(selection)

      // Update the tally
      updateTally(outcome);

      // Wait before displaying the next button
      setTimeout(function (){nextButton()}, 5000);

      return(response)
    };

    // Function that updates the tally
    function updateTally(outcome) {
      totalPoints += parseInt(outcome);

      // Draw tally using HTML
      tally = display_element.querySelector(".tally");
      tally.innerHTML = `Total Points: ${totalPoints}`

    }

    // Function that displays the number of points won
    function feedbackCue(selection){
      if(selection == 1){
        // Add option to CSS classes that change opacity of the stimuli (same below)
        stimulus1.classList.add('chosen');
        stimulus2.classList.add('notChosen');
        // Draw feedback using HTML
        option1.innerHTML == "img src = 'cues/Red.png'></img>";
        } else if(selection == 2){
          stimulus2.classList.add('chosen');
          stimulus1.classList.add('notChosen');
          option2.innerHTML == "img src= 'cues/Red.png'></img>";
        }

        // Save selection and outcome for trial data
        response.choice = selection;
        response.feedback = outcome;
    };

    // Draw the outcome from the probability distribution
    function outcomeDiscrete(selection) {

      if (selection == 1){
        if (trial.outcomes1.length == undefined){ // If there's only one possible outcome (same below)
          outcome = trial.outcomes1;
        } else {
          outcome = jsPsych.randomization.sampleWithReplacement(trial.outcomes1, 1, trial.prob1);
        }
      } else if (selection == 2){
        if (trial.outcomes2.length == undefined){
          outcome = trial.outcomes2;
        } else {
          outcome = jsPsych.randomization.sampleWithReplacement(trial.outcomes2, 1, trial.prob2);
        }
      }

      return(outcome)
    }

    // Function to draw the next button and to end the trial when participants click on it
    function nextButton() {

      next.innerHTML += `<button id="next-button" class="next-button hvr-grow" draggable="false">NEXT</button>`;
      next.addEventListener("click", function (){
        endTrial();
      });
    }

    // What to do at the end of the trial
    function endTrial() {

      // Kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // Remove from bandit styles css (in case the next trial uses standard jspsych.css)
      content.classList.remove("bandit-grid-container")

      // Save data
      var trial_data = {
        banditTrial: trialNumber,
        choice: response.choice,
        feedback: response.feedback,
        rt: response_time
      };

      // Clear the display
      display_element.innerHTML = '';

      // End trial
      jsPsych.finishTrial(trial_data);

    };

    var response_time = 0;
    const startTime = (new Date()).getTime();
  };

  return plugin;
})();
