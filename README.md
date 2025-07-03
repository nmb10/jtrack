# jtrack
Minimalistic local app to track time on jira issues. Displays issues with start timer, stop timer buttons and ability to log time to jira.

## Installation:

Note: Installation requires erlang, recommended version is Erlang/OTP 25

1. Clone the repo

2. Run app
```bash
cd ./jtrack
make run
```

3. Open http://localhost:8087/ in browser.

## Initial configuration:
- Set `Jira Bearer Token`
- Set `Jira Base Url`
- Click `Check connection` to verify creds are correct. FIXME: Implement
- Click `Fetch issues` FIXME: Implement.
List of your issues should be loaded now and the app is ready to track Jira issues.

## Usage:
- Click 'Toggle' on the issue you want to track (or 'w');
- When finish to work on the issue, click 'Toggle' again (or 'w') to stop to track that issue or click on other issue's 'Toggle' (or 'w');
- When the day work is finished, click 'Save' on the issue on the history (left) side to save the issue time to Jira.
