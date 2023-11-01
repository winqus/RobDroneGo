import { exec } from 'node:child_process';

let childProcess = null;
let sigintReceived = false;

function execPromise(command) {
  return new Promise((resolve, _reject) => {
    childProcess = exec(command);

    childProcess.stdout.on('data', (data) => {
      console.log(data.toString());
    });

    childProcess.stderr.on('data', (data) => {
      console.error(data.toString());
    });

    childProcess.on('close', (code) => {
      childProcess = null;
      resolve();
    });
  });
}

async function runScript(script) {
  try {
    await execPromise(`npm run ${script}`);
  } catch (error) {
    console.error(error.message);
  }
}

async function handleSigint() {
  console.log('\nShutting down and cleaning up due to SIGINT (Ctrl+C)');
  sigintReceived = true;

  if (childProcess) {
    childProcess.kill('SIGINT');
  }

  await runScript('e2eTest:physicalTeardown');
}

process.on('SIGINT', handleSigint);

(async () => {
  await runScript('e2eTest:setEnv');
  await runScript('e2eTest:start');

  if (!sigintReceived) {
    await runScript('e2eTest:physicalTeardown');
  }
})();
