const jiraBaseUrl = ''  // FIXME: Take from server side.
const sessionColors = [
  '#d33682',
  '#93a1a1',
  '#b58900',
  '#6c71c4',
  '#586e75',
  '#657b83',
  '#dc322f',
  '#268bd2',
  '#839496',
  '#2aa198',
  '#859900',
  '#d33682',
  '#93a1a1',
  '#b58900',
  '#6c71c4',
  '#586e75',
  '#657b83',
  '#dc322f'
]

window.onload = () => {

  let featuredIssues = []
  let issues = []
  let currentIssue = null
  let inProgressIssue = null
  let stat = {}
  let jtrackWorklog = {}
  let rowIndex = -1

  document.getElementById('app').addEventListener('issues.fetch', function (event) {
    const requestOptions = {}

    fetch('/api/v1/issues', requestOptions)
      .then(response => response.json())
      .then(data => {
        inProgressIssue = data.current_issue
        issues = data.issues
        featuredIssues = data.featured_issues
        stat = data.stat
        jtrackWorklog = data.jtrack_worklog
        if (currentIssue === null) {
          currentIssue = issues[0]
        }
        render(featuredIssues, issues, inProgressIssue,
               rowIndex, stat, jtrackWorklog)
        const displayLines = function() {
          // Remove all svg nodes.
          //
          var elements = document.getElementsByTagName('svg')
          while (elements[0]) elements[0].parentNode.removeChild(elements[0])

          let sessionId
          let coloredId
          let startElement, endElement
          let elem
          const collection = document.getElementsByClassName('worklog-session-li')
          for (let i = 0; i < collection.length; ++i) {
            elem = collection[i]
            sessionId = elem.id.replace('lid-', '')
            coloredId = 'work-session-' + sessionId
            startElement = document.getElementById('lid-' + sessionId)
            endElement = document.getElementById(coloredId)
            colorIndex = collection.length - 1 - i
            new LeaderLine(startElement, endElement, {color: sessionColors[colorIndex], size: 8})
          }
        }
        setTimeout(displayLines, 2000)
      })
  })

  document.addEventListener("keydown", function(event) {
    if (event.which === 74) {
      // j, move down
      rowIndex += 1
    } else if (event.which === 75) {
      // k, move up
      rowIndex -= 1
    } else if (event.which === 72) {
      // h, move left
      console.log('Move status to right (+1)')
    } else if (event.which === 76) {
      // l, move right
      console.log('Move status to left (-1)')
    } else if (event.which === 87) {
      // w, toggle
      if (currentIssue) {
        toggleIssue(currentIssue.key, currentIssue.title, currentIssue.status)
      }
    } else {
      console.log(event.which)
    }

    if (rowIndex < 0) {
      rowIndex = 0
    }

    if (rowIndex > document.querySelectorAll("#app table tr").length) {
      rowIndex = document.querySelectorAll("#app table tr").length
    }

    currentIssue = issues[rowIndex]

    render(featuredIssues, issues, inProgressIssue, rowIndex, stat, jtrackWorklog)
  }) // onkeydown finish

  renderConfig()

  const fire = function() {
    document.getElementById('app').dispatchEvent(
      new CustomEvent(
        'issues.fetch',
        {}))
  }
  setInterval(fire, 10000)
  fire()
}

const renderConfig = function() {
  let configWidget = document.querySelector("#config")
  const dailyLimit = 8  // FIXME: Take from server side.
  const jiraBearer = 'ND***'  // FIXME: Implement
  const content = `
    <ul>
      <li>Featured issues: <textarea>FIXME: Implement me</textarea></li>
      <li>
        Daily limit (stop tracker when the limit reached):
        <input type="number" value="${dailyLimit}" />
      </li>
      <li>
        <label for="jira-upgrade">Log hours to jira: FIXME:</label>
        <select name="jira-upgrade" id="jira-upgrade">
          <option value="on-daily-limit-reach">on daily limit reach</option>
          <option value="manually">manually</option>
        </select>
      </li>
      <li>Jira bearer: <input type="text" value="${jiraBearer}" disabled/></li>
      <li>Jira base url: <input type="text" value="${jiraBaseUrl}" disabled/></li>
    </ul>
  `
  configWidget.innerHTML = content
}

const renderSession = function(date, session, elemIndex, totalElems) {
  // session example:
  // [ "JT-11", [ "2025-03-07T08:22:28", 123 ],
  [issueKey, [sessionId, sessionStartTime, sessionMinutes, sessionSaved]] = session
  let ret
  const hours = Math.floor(sessionMinutes / 60)
  let minutes = sessionMinutes % 60
  const issueTitle = 'FIXME: ...'  // FIXME: Implement first 10 chars.
  const colorIndex = totalElems - 1 - elemIndex
  if (minutes === 0) {
      ret = ''
  } else {
    if (sessionSaved) {
      ret = `
        <li style="border-left: 10px ${sessionColors[colorIndex]} solid; border-top: 10px ${sessionColors[colorIndex]} solid" id="lid-${sessionId}" class="worklog-session-li">
          <a href="https://${jiraBaseUrl}/browse/${issueKey}">${issueKey}</a>: ${issueTitle} | ${hours}h. ${minutes}m. &#9989;
        </li>
      `
    } else {

      ret = `
        <li style="border-left: 10px ${sessionColors[colorIndex]} solid; border-top: 10px ${sessionColors[colorIndex]} solid" id="lid-${sessionId}" class="worklog-session-li">
          <div style="float:left">
            <button style="padding:0px;margin:0;width:25px;height:30px;">&uarr;</button>
            <br />
            <button style="padding:0px;margin:0;width:25px;height:30px;">&darr;</button>
          </div>
          <div>
            <a href="https://${jiraBaseUrl}/browse/${issueKey}">
              ${issueKey}
            </a>
            : ${issueTitle} | ${sessionStartTime}/${hours}h. ${minutes}m.
            <button title="Save session time to jira"
                    onclick="saveSession('${sessionId}', '${sessionStartTime}', ${sessionMinutes}, '${issueKey}')">
              Save
            </button>
          </div>
        </li>
      `
    }
  }
  return ret
}

const renderWeekDaySessions = function(date, sessions) {
  // sessions example:
  // [ [ "JT-11", [ "2025-03-07T08:22:28", 123 ] ],
  //
  const jtrackWorklogItemsContent = sessions.map((elem, index) => renderSession(date, elem, index, sessions.length)).join("\n")
  let issueBlocks = []

  let blockWidth
  let temp
  let issue
  let hours
  let minutes
  let colorIndex
  for (const [i, workSession] of sessions.entries()) {
      totalMinutes = workSession[1][2]
      hours = Math.floor(totalMinutes / 60)
      minutes = totalMinutes % 60
      blockWidth = totalMinutes
      issue = workSession[0]
      colorIndex = sessions.length - 1 - i
      temp = `<div title="${issue} (${hours}h ${minutes}m)" id="work-session-${workSession[1][0]}" style="float:left; width: ${blockWidth}px;height:40px;border:2px black solid; background-color: ${sessionColors[colorIndex]}">&nbsp;</div>`
      issueBlocks = [temp, ...issueBlocks]
  }

  const coloredBlocksContent = issueBlocks.join('\n')
  let ret = '<div>---</div>'
  if (sessions.length > 0) {
    ret = `
      <div style="height:40px;width:490px;padding-left:4px;padding-right:4px;background-color:gray">
        ${coloredBlocksContent}
      </div>
      <ul>
        ${jtrackWorklogItemsContent}
      </ul>
    `
  }
  return ret
}

const renderWeekDay = function(weekDayStat) {
  // weekDayStat example:
  // ["2025-03-07", [ [ "JT-12", [ "2025-03-07T08:22:28", 123 ] ],
  //
  const date = weekDayStat[0]
  const dateStat = weekDayStat[1]
  let dayTotalMinutes = 0
  for (const issueLog of dateStat) {
    dayTotalMinutes += issueLog[1][2]
  }

  const dayHours = Math.floor(dayTotalMinutes / 60)
  let dayMinutes = dayTotalMinutes % 60
  const formattedDate = formatDate(new Date(date))

  const weekDaySessions = renderWeekDaySessions(date, dateStat)

  let weekAction
  const hasUnsavedIssue = weekDayStat[1].some(x => x[1][3] == false)
  if (weekDayStat[1].length == 0) {
    weekAction = ''
  } else if (hasUnsavedIssue) {
    weekAction = `
      <button title="Save all unsaved sessions to jira"
              onclick="saveDay('${date}')">
        Save (FIXME:)
      </button>
    `
  } else {
    // All issues for that day are saved.
    weekAction = "&#9989;"
  }
  const ret = `
    <ul>
      <li>
        ${formattedDate} (${dayHours}h. ${dayMinutes}m.)${weekAction}
        ${weekDaySessions}
      </li>
    </ul>
  `
  return ret
}

const renderIssue = function(issue, inProgressIssue, isCurrent) {
  let prefix = ''
  let background = ''
  let border = 'border: 3px solid white; border-radius: 4px;'
  if (inProgressIssue && issue.key === inProgressIssue.key) {
    background = "background-color: #2aa198"
  }
  if (isCurrent) {
    border = "border: 3px solid gray; border-radius: 4px;"
  }
  return `
    <tr>
      <td style="${background}; ${border}">
        <a href="https://${jiraBaseUrl}/browse/${issue.key}">
          ${issue.key}
        </a>: ${issue.title}
        <button onclick="toggleIssue('${issue.key}', '${issue.title}', '${issue.status}')">
          Toggle
        </button>
      </td>
    </tr>
  `
}


const toggleIssue = function(issueKey, issueTitle, issueStatus) {

  const requestOptions = {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      key: issueKey,
      title: issueTitle,
      status: issueStatus})
  }

  fetch('/api/v1/issues', requestOptions)
    .then(response => response.json())
    .then(data => {
      document.getElementById('app')
        .dispatchEvent(new CustomEvent('issues.fetch', {}))
    })
}

const saveDay = function(date) {
  console.log('FIXME:', date)
}

const saveSession = function(sessionId, startedAt, minutes, issueKey) {
  /*
   * Saves issue time to jira.
   *
   */

  const requestOptions = {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      session_id: sessionId,
      started_at: startedAt,
      issue_key: issueKey,
      minutes: minutes})
  }
  fetch('/api/v1/sessions', requestOptions)
    .then(response => response.json())
    .then(data => {
      document.getElementById('app')
        .dispatchEvent(new CustomEvent('issues.fetch', {}))
    })
}

const render = function(featuredIssues, issues, inProgressIssue,
                        currentRowIndex, stat, jtrackWorklog) {
  if (inProgressIssue) {
    changeFavicon('⏲')
  } else {
    changeFavicon('🏝')
  }
  let app = document.querySelector("#app")

  const requestOptions = {}
  const currentIssue = null

  const issuesBlock = issues.map((issue, i) => renderIssue(issue, inProgressIssue, i == currentRowIndex)).join('\n')
  const featuredIssuesBlock = featuredIssues.map((issue, i) => renderIssue(issue, inProgressIssue, i == currentRowIndex)).join('\n')
  const issuesTable =  `<table>
    <tbody style="background-color: #daebe3">
      <tr style="background-color: #e4f0f5; border-radius: 4px;">
        <th colspan="1">Featured issues:</th>
      </tr>
      ${featuredIssuesBlock}
    </tbody>
    <tbody>
      <tr style="background-color: #e4f0f5; border-radius: 4px;">
        <th colspan="1">My issues:</th>
      </tr>
      ${issuesBlock}
    </tbody>
  </table>`
  let totalMinutes = 0
  for (const mins of Object.values(stat)) {
    totalMinutes += mins
  }
  const todayHours = Math.trunc(totalMinutes / 60)
  const todayMinutes = totalMinutes % 60
  let todayStatBlock
  if (inProgressIssue === null) {
    todayStatBlock = `<h2>Today: ${todayHours}h ${todayMinutes}m</h2>`
  } else {
    todayStatBlock = `<h2 style="background-color: #859900; border: 3px solid white; border-radius: 4px;">Today: ${todayHours}h ${todayMinutes}m</h2>`
  }

  let jtrackWeekAmount = 0
  let jtrackPreviousWeekAmount = 0
  let jiraWeekAmount = 0
  let jiraPreviousWeekAmount = 0

  const thisWeekStartDate = new Date()
  const distance = thisWeekStartDate.getDay() - 1
    thisWeekStartDate.setDate(thisWeekStartDate.getDate() - distance)
    thisWeekStartDate.setHours(0, 0, 0, 0)

  const previousWeekStartDate = new Date()
  const distance1 = thisWeekStartDate.getDay() - 8
  previousWeekStartDate.setDate(thisWeekStartDate.getDate() + distance1)
  previousWeekStartDate.setHours(0, 0, 0, 0)

  let thisWeekIssuesContent = []
  let previousWeekIssuesContent = []
  let jtrackDate
  let jiraLogged
  let jiraStartTime
  for (let jtrackWorklogItem of jtrackWorklog) {
    jtrackDate = new Date(jtrackWorklogItem[0])
    if (jtrackDate >= thisWeekStartDate) {
      for (let session of jtrackWorklogItem[1]) {
        jtrackWeekAmount += session[1][2]
        jiraLogged = session[1][3]
        if (jiraLogged) {
          jiraWeekAmount += session[1][2]
        }
      }
      thisWeekIssuesContent.push(renderWeekDay(jtrackWorklogItem))
    } else if (jtrackDate >= previousWeekStartDate) {
      for (let session of jtrackWorklogItem[1]) {
        jtrackPreviousWeekAmount += session[1][2]
        jiraLogged = session[1][3]
        if (jiraLogged) {
          jiraWeekAmount += session[1][2]
        }
      }
      previousWeekIssuesContent.push(renderWeekDay(jtrackWorklogItem))
    }
  }

  const jtrackWeekHours = Math.floor(jtrackWeekAmount / 60)
  const jtrackWeekMinutes = jtrackWeekAmount % 60

  const jtrackPreviousWeekHours = Math.floor(jtrackPreviousWeekAmount / 60)
  const jtrackPreviousWeekMinutes = jtrackPreviousWeekAmount % 60

  const jiraWeekHours = Math.floor(jiraWeekAmount / 60)
  const jiraWeekMinutes = jiraWeekAmount % 60

  const jiraPreviousWeekHours = Math.floor(jiraPreviousWeekAmount) / 60
  const jiraPreviousWeekMinutes = jiraPreviousWeekAmount % 60

  const thisWeekStatContent = `
    <h3>This week (jtrack): ${jtrackWeekHours}h ${jtrackWeekMinutes}m</h3>
    <h3>This week (jira): ${jiraWeekHours}h ${jiraWeekMinutes}m</h3>
  `

  const previousWeekStatContent = `
    <h3>Previous week (jtrack): ${jtrackPreviousWeekHours}h ${jtrackPreviousWeekMinutes}m</h3>
    <h3>Previous week (jira): ${jiraPreviousWeekHours}h ${jiraPreviousWeekMinutes}m</h3>
  `

  const content = `
    <div class="row">
      <div class="column column-50">
        ${thisWeekStatContent}
        ${thisWeekIssuesContent.join("\n")}
        <hr />
        ${previousWeekStatContent}
        ${previousWeekIssuesContent.join("\n")}
      </div>
      <div class="column column-50">
        ${todayStatBlock}
        ${issuesTable}
      </div>
    </div>
  `

  app.innerHTML = content
}


const formatDate = function(date) {
  const options = {
    weekday: "short",
    year: "numeric",
    month: "short",
    day: "numeric",
  }
  return date.toLocaleDateString('en-us', options)
}

function changeFavicon(text) {
  const canvas = document.createElement('canvas')
  canvas.height = 64
  canvas.width = 64
  const ctx = canvas.getContext('2d')
  ctx.font = '64px serif'
  ctx.fillText(text, 0, 64)

  const link = document.createElement('link')
  const oldLinks = document.querySelectorAll('link[rel="shortcut icon"]')
  oldLinks.forEach(e => e.parentNode.removeChild(e))
  link.id = 'dynamic-favicon'
  link.rel = 'shortcut icon'
  link.href = canvas.toDataURL()
  document.head.appendChild(link)
}
