# jtrack
Minimalistic time tracking tool app for jira issues. Just start timer,
stop timer and log time to jira.

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
- Click `Check connection` to verify creds are correct.
- Click `Fetch issues`
List of your issues should be loaded now and the app is ready to track Jira issues.

## Usage:
- Click 'Toggle' on the issue you want to track;
- When finish to work on the issue, click 'Toggle' again to stop to track that issue or click on other issue's 'Toggle';
- When the day work is finished, click 'Save' on the issue to save the issue time to Jira.
