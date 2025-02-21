const { MongoClient } = require('mongodb');

const dbUri = 'mongodb://127.0.0.1:27017';

async function teardownDatabases() {
  const client = new MongoClient(dbUri, { useNewUrlParser: true, useUnifiedTopology: true });

  try {
    await client.connect();

    // List all databases
    const databasesList = await client
      .db()
      .admin()
      .listDatabases();

    // Filter databases based on the specified format
    const testDatabases = databasesList.databases.filter((db) => db.name.match(/^test-\d{8}T\d{9}Z$/));

    // Drop each test database
    for (const db of testDatabases) {
      await client.db(db.name).dropDatabase();
      console.log(`Dropped database: ${db.name}`);
    }
  } catch (err) {
    console.error('An error occurred while tearing down the test databases:', err);
  } finally {
    await client.close();
  }
}

teardownDatabases();
