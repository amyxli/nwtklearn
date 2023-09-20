
jsPsych.plugins["bandit-task-sounds"] = (function() {

  var plugin = {};

  plugin.info = {
    name: "bandit-task",
    description: "Bandit task with contingent cues.",
    parameters: {
      stimulus1: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: "Option 1",
        default_value: undefined,
        description: "Image filename for Stimulus 1."
      },
      stimulus2: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: "Option 2",
        default_value: undefined,
        description: "Image filename for Stimulus 2."
      },
      cue1: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: "Cue 1",
        default_value: undefined,
        description: "Array of image filenames for cues associated with Stimulus 1."
      },
      cue2: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: "Cue 2",
        default_value: undefined,
        description: "Array of image filenames for cues associated with Stimulus 2."
      },
      cueProbs1: {
        type: jsPsych.plugins.parameterType.FLOAT,
        pretty_name: "Option 1 Cue Probabilities",
        default_value: null,
        description: "Probabilities associated with the cues for Stimulus 1. Must be an array of the same length as cue1."
      },
      cueProbs2: {
        type: jsPsych.plugins.parameterType.FLOAT,
        pretty_name: "Option 2 Cue Probabilities",
        default_value: null,
        description: "Probabilities associated with the cues for Stimulus 2. Must be an array of the same length as cue2."
      },
      outcomes1: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Option 1 Outcomes",
        default_value: undefined,
        description: "An array containing arrays of outcomes associated with each cue for Stimulus 1."
      },
      outcomes2: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Option 2 Outcomes",
        default_value: undefined,
        description: "An array containing arrays of outcomes associated with each cue for Stimulus 2."
      },
      outcomeProbs1: {
        type: jsPsych.plugins.parameterType.FLOAT,
        pretty_name: "Option 1 Outcomes Probabilities",
        default_value: null,
        description: "Probabilities associated with the outcomes for Stimulus 1. Must be an array of the same length as outcomes1."
      },
      outcomeProbs2: {
        type: jsPsych.plugins.parameterType.FLOAT,
        pretty_name: "Option 2 Outcomes Probabilities",
        default_value: null,
        description: "Probabilities associated with the outcomes for Stimulus 2. Must be an array of the same length as outcomes2."
      },
        cueDuration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Feedback Duration",
        default_value: 2000,
        description: "How long (ms) to display the cue before feedback appears."
      },
       feedbackDuration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Feedback Duration",
        default_value: 2000,
        description: "How long (ms) to display feedback (reward) before next appears."
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
    var responses = {
      banditTrial: null,
      choice: null,
      cue: null,
      feedback: null,
      rt: null
    };

    // Update trial number
    trialNumber++;
    responses.banditTrial = trialNumber;

    //trial number tally update after practice trials
    if (trialNumber == 7) {
      totalPoints = 3000
    } else {
      totalPoints = totalPoints
    };

    // Add content to css class in order to apply bandit styles
    const content = document.querySelector("#jspsych-content");
    content.classList.add("bandit-grid-container");

    // Set up CSS grid container in HTML
    display_element.innerHTML = `

        <div id="leftOption" class="grid-item options"></div>
        <div id="rightOption" class="grid-item options"></div>
        <div id="centred" class="grid-item centred"></div>
        <div id="next" class="grid-item next"></div>
        <div id="lossBlock" class="grid-item winBlock"></div>`;

// aversive audio

  display_element.innerHTML += `
    <audio id="mic">
    <source src="sounds/mic.mp3" type="audio/mpeg">
    </audio`;

  var audio = display_element.querySelector('#mic');

    // Make variables for the options and next button
    var leftOption = display_element.querySelector("#leftOption");
    var rightOption = display_element.querySelector("#rightOption");
    var centred = display_element.querySelector("#centred");
    var next = display_element.querySelector("#next");
    var outcome = null;
    var lossBlock = display_element.querySelector('#lossBlock');

    // Display choice stimuli when pre-trial interval has elapsed
    if(trial.preTrialInterval == 0){
      stimuli = randomiseSide();
      displayChoice();
    } else {
      jsPsych.pluginAPI.setTimeout(function(){
        stimuli = randomiseSide();
        displayChoice();
      }, trial.preTrialInterval);
    }

// FUNCTIONS -------------------------------------------------------------------------------------------------------

    // Function to randomise side --------------------------------------------------------------
    function randomiseSide() {
      var unordered = [trial.stimulus1, trial.stimulus2];
      var order = jsPsych.randomization.sampleWithoutReplacement([0, 1], 2);

      var stimuli = {
        order: order,
        leftOption: unordered[order[0]],
        rightOption: unordered[order[1]]
      };
      return stimuli
    }


    // Funciton to draw stimuli to the screen and start listening for clicks----------------------
    function displayChoice() {

      //draw loss block instruction
      lossBlock.innerHTML = `Sound Trial`;

      // Draw stimuli using HTML
      leftOption.innerHTML = `<img id="stimulus${(stimuli.order[0]+1)}" class="options" src="${stimuli.leftOption}" draggable="false">`;
      rightOption.innerHTML = `<img id="stimulus${(stimuli.order[1]+1)}" class="options" src="${stimuli.rightOption}" draggable="false">`;

      // Select the stimuli so you can interact with them
      stimulus1 = display_element.querySelector("#stimulus1");
      stimulus2 = display_element.querySelector("#stimulus2");

      // Start "listening" for the participant clicking the stimulus
      // When they click on it, run the function chooseOption
      stimulus1.addEventListener("click", chooseStimulus1);
      stimulus2.addEventListener("click", chooseStimulus2);

    };

    // Functions for when the participant chooses each option -----------------------------------
    // Necessary because you can't pass parameters directly into addEventListener (above)
    function chooseStimulus1() {makeChoice(selection=1)}
    function chooseStimulus2() {makeChoice(selection=2)}

    // What happens when the participant clicks the stimulus -------------------------------------
    function makeChoice(selection) {

      // Calculate reaction time
      const endTime = (new Date()).getTime();
      responses.rt = endTime - startTime;

      // Stop listening for clicks (so people can't select more than one option)
      stimulus1.removeEventListener("click", chooseStimulus1);
      stimulus2.removeEventListener("click", chooseStimulus2);

      // remove loss block instruction
      lossBlock.innerHTML = `  `;

      // Display cue
      const displayedCue = cueHTML(selection);
      responses.cue = parseInt(displayedCue.index) + 1;

      // Calculate outcomes
      outcome = calculateOutcome(selection, displayedCue);

      // Wait before displaying feedback
      setTimeout(function (){

      // Draw feedback to screen
     feedbackHTML(selection);


      // Update the tally
  //  updateTally(outcome);

      // Wait before displaying the next button
      setTimeout(function (){nextButton()}, trial.feedbackDuration);

      }, trial.cueDuration);

    };

    // Function that updates the tally --------------------------------------------------------------
  /*  function updateTally(outcome) {
      totalPoints += parseInt(outcome);

      // Draw tally using HTML
      tally = display_element.querySelector(".tally");
      tally.innerHTML = `Total Points: ${totalPoints}`

    } */

    // Function that displays the cue --------------------------------------------------------------
    function cueHTML(selection){
      // Hide stimuli
      leftOption.style.display = "none";
      rightOption.style.display = "none";

      // Randomly select cue
      const cue = randomiseCue(selection);

      // Draw cue to screen using HTML
      if(selection == 1){
        centred.innerHTML = `<div id='cue' class='cue centred'><img id="cueImage" class="cue" src="${cue.image}" draggable="false"></div>`;
      } else if(selection == 2){
        centred.innerHTML = `<div id='cue' class='cue centred'><img id="cueImage" class="cue" src="${cue.image}" draggable="false"></div>`;
      }

      return cue
    };

    // Function to select a random cue --------------------------------------------------------------
    function randomiseCue(selection) {
      let cue = {
        index: [],
        image: ""
      };

      let cueSeq = [];
      let cueIndex = [];

      if (selection == 1){
        if (trial.cue1.length == undefined){ // If there's only one possible outcome (same below)
          cueIndex = 0;
        } else {
          for(let i = 0; i < trial.cue1.length; i++) { cueSeq.push(i); }
          cueIndex = jsPsych.randomization.sampleWithReplacement(cueSeq, 1, trial.cueProbs1);
        }
        cue.image = trial.cue1[cueIndex];
      } else if (selection == 2){
        if (trial.cue2.length == undefined){
          cueIndex = 0;
        } else {
          for(let i = 0; i < trial.cue2.length; i++) { cueSeq.push(i); }
          cueIndex = jsPsych.randomization.sampleWithReplacement(cueSeq, 1, trial.cueProbs2);
        }
        cue.image = trial.cue2[cueIndex];
      }

      cue.index = cueIndex;

      return(cue)
    };


    // Function that displays the number of points won --------------------------------------------
    function feedbackHTML(selection){

      // Select the cue and then hide it
      var cue = display_element.querySelector(".cue");
      cue.style.display = "none";

      if(outcome == -100 | outcome == -110 | outcome == -90 | outcome == -101 | outcome == -102 | outcome == -103 | outcome == -104 | outcome == -105
      | outcome == -106 | outcome == -107 | outcome == -108 | outcome == -109 | outcome == -91 | outcome == -92 | outcome == -93 | outcome == -94 | outcome == -95  | outcome == -96
    | outcome == -97 | outcome == -98 | outcome == -99){
        // Draw feedback using HTML
        centred.innerHTML = "<h1> Feedback </h1>";
      } else {
        centred.innerHTML = "<h1> No Feedback </h1>";
      }

      if (outcome == -100 | outcome == -110 | outcome == -90 | outcome == -101 | outcome == -102 | outcome == -103 | outcome == -104 | outcome == -105
      | outcome == -106 | outcome == -107 | outcome == -108 | outcome == -109 | outcome == -91 | outcome == -92 | outcome == -93 | outcome == -94 | outcome == -95  | outcome == -96
    | outcome == -97 | outcome == -98 | outcome == -99){
        audio.play();
      }

        // Save selection and outcome for trial data
        responses.choice = selection;
        responses.feedback = outcome;
    };

    // Draw the outcome from the probability distribution ---------------------------------------------
    function calculateOutcome(selection, displayedCue) {

      if (selection == 1){
        if (trial.outcomes1[displayedCue.index].length == undefined){ // If there's only one possible outcome (same below)
          outcome = trial.outcomes1[displayedCue.index];
        } else {
          outcome = jsPsych.randomization.sampleWithReplacement(trial.outcomes1[displayedCue.index], 1, trial.outcomeProbs1);
        }
      } else if (selection == 2){
        if (trial.outcomes2[displayedCue.index].length == undefined){
          outcome = trial.outcomes2[displayedCue.index];
        } else {
          outcome = jsPsych.randomization.sampleWithReplacement(trial.outcomes2[displayedCue.index], 1, trial.outcomeProbs2);
        }
      }

    /*  if (outcome == 100){
        var trial = {
        type: 'audio-keyboard-response',
        stimulus: 'sounds/Tada.mp3',
        choices: jsPsych.NO_KEYS,
        trials_ends_after_audio: true
      }}
      else {
        return(outcome)
      }; */

      var randomArray = [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

      if (outcome == 100 | outcome == -100){
      outcome = parseInt(outcome) + parseInt(jsPsych.randomization.sampleWithReplacement(randomArray, 1));
      }

      return(outcome)
    }

    // Function to draw the next button and to end the trial when participants click on it ----------------------
    function nextButton() {

      next.innerHTML = `<button id="next-button" class="next-button hvr-grow" draggable="false">NEXT</button>`;
      next.addEventListener("click", function (){
        endTrial();
      });
    }

    // What to do at the end of the trial --------------------------------------------------------------------
    function endTrial() {

      // Kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // Remove from bandit styles css (in case the next trial uses standard jspsych.css)
      content.classList.remove("bandit-grid-container")

      // Clear the display
      display_element.innerHTML = '';

      // End trial and save responses
      jsPsych.finishTrial(responses);

    };

    const startTime = (new Date()).getTime();
  };

  return plugin;
})();
