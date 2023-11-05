import spawn from 'cross-spawn';

let childProcess = null;
let sigintReceived = false;

function spawnPromise(script, envVars = {}) {
  return new Promise((resolve, reject) => {
    const env = { ...process.env, ...envVars };
    const npmCmd = process.platform === 'win32' ? 'npm.cmd' : 'npm';
    const args = ['run', script];

    // Use cross-spawn to handle the script execution
    childProcess = spawn(npmCmd, args, { stdio: 'inherit', env });

    childProcess.on('exit', (code) => {
      childProcess = null;
      if (code === 0) {
        resolve();
      } else {
        const error = new Error(`Script "${script}" exited with code ${code}`);
        error.code = code;
        reject(error);
      }
    });

    childProcess.on('error', (err) => {
      childProcess = null;
      reject(err);
    });
  });
}

async function runScript(script, envVars = {}) {
  try {
    await spawnPromise(script, envVars);
  } catch (error) {
    console.error(`Script "${script}" run disturbed. `, error.message);
  }
}

async function handleSigint() {
  console.log('\nShutting down and cleaning up due to SIGINT (Ctrl+C).');
  sigintReceived = true;

  if (childProcess && !childProcess.killed) {
    childProcess.kill('SIGINT');
  }

  await runScript('e2eTest:physicalTeardown').catch((error) => {
    console.error('Error during shutdown:', error.message);
  });

  process.exit();
}

process.on('SIGINT', handleSigint);

(async () => {
  await runScript('e2eTest:setEnv');

  await runScript('e2eTest:start', { NODE_ENV: 'test' });

  if (!sigintReceived) {
    await runScript('e2eTest:physicalTeardown');
  }
})();
