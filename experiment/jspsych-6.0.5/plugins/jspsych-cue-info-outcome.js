
jsPsych.plugins["cue-info-outcome"] = (function() {

    var plugin = {};

    plugin.info = {
      name: "cue-info-outcome",
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
        PredictCue1: {
          type: jsPsych.plugins.parameterType.IMAGE,
          pretty_name: "Predict Cue One",
          default_value: undefined,
          description: "One of two cues which predicts reward when choosing the Find Out Now option."
        },
        PredictCue2: {
          type: jsPsych.plugins.parameterType.IMAGE,
          pretty_name: "Predict Cue Two",
          default_value: undefined,
          description: "One of two cues which predicts reward when choosing the Find Out Now option."
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

  //------------------------------------------------------------------------------------------------
  plugin.trial = function(display_element, trial) {

    let stimulus1 = null;
    let stimulus2 = null;
  // Variable to store trial trial_data
  var response = {
    choice: null,
    feedback: null,
    rt: null
  };

  //update trial number
  //trialNumber++;

  //add content to css class in order to apply bandit style
  // const content = document.querySelector("#jspsych-content");
  // content.classList.add("bandit-grid-container");

  //Set up CSS grid container in html
  display_element.innerHTML = `
      <div class="grid-item tally">Total Points: ${totalPoints}</div>
      <div id="option1" class="grid-item options"></div>
      <div id="option2" class="grid-item options"></div>
      <div class="grid-item next"></div>`;

  // Make variables for the options, cues, and outcomes
  var option1 = document.querySelector("#option1");
  var option2 = document.querySelector("#option2");

  // Display choice stimuli once pre-trial interval has elapsed
  if(trial.preTrialInterval == 0){
    displayChoice();
  } else {
    jsPsych.pluginAPI.setTimeout(function(){
      displayChoice();
    }, trial.preTrialInterval);
  }

//FUNCTIONS -------------------------------------------------------------------------------------------

  function displayChoice () {

    //Draw stimuli options

    option1.innerHTML = `<img id="stimulus1" class="stimuli" src="${trial.stimulus1}" draggable="false">`;
    option2.innerHTML = `<img id="stimulus2" class="stimuli" src="${trial.stimulus2}" draggable="false">`;

    //select stimuli
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

    // Calculate and display cue of choice
    cue = outcomeDiscrete(selection);
    feedbackCue(selection)

    //wait before displaying the final outcome
    setTimeout(function (){displayCue()}, 5000);

    //don't think necessary
    return(response)
    };


  // Function that displays the cue
    function feedbackCue(selection){
      if(selection == 1){
        //draw cue using IMG
        option1.innerHTML += `<div id='feedback${selection}' class='feedback'><p>${cue} points</p></div>`;
      } else if(selection == 2){
        option2.innerHTML += `<div id='feedback${selection}' class='feedback'><p>${cue} points</p></div>`;
      }
      // Save selection and cue for trial data
      response.choice = selection;
      response.feedback = cue;
    };

    // Draw the outcome from the probability distribution
    function outcomeDiscrete(selection) {
      if(selection == 1){
        if (trial.outcomes1.length == undefined){ // If there's only one possible outcome (same below)
          cue = trial.outcomes1;
        } else {
          cue = jsPsych.randomization.sampleWithReplacement(trial.outcomes1, 1, trial.prob1);
        }
      }

      return(cue)
    }


    // What to do at the end of the trial
    function endTrial() {

      // Kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // Remove from bandit styles css (in case the next trial uses standard jspsych.css)
      content.classList.remove("bandit-grid-container")

      // Save data
      var trial_data = {
        //banditTrial: trialNumber,
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
