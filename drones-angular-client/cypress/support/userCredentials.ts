// These are based on the users that are seeded to db for e2e tests in Node API (see scripts/ folder)

const simpleUserCreds = {
  firstName: 'JustUser',
  lastName: 'Lastname',
  email: 'iuser@isep.ipp.pt',
  password: 'GoodPass!1',
};

const systemAdminCreds = {
  firstName: 'SysAdmin',
  lastName: 'Lastname2',
  email: 'isystemadmin@isep.ipp.pt',
  password: 'GoodPass!1',
};

const taskManagerCreds = {
  firstName: 'MeTaskManager',
  lastName: 'Lastname3',
  email: 'itaskmanager@isep.ipp.pt',
  password: 'GoodPass!1',
};

const campusManagerCreds = {
  firstName: 'MeCampusManager',
  lastName: 'Lastname4',
  email: 'icampus@isep.ipp.pt',
  password: 'GoodPass!1',
};

const fleetManagerCreds = {
  firstName: 'MeFleetManager',
  lastName: 'Lastname5',
  email: 'ifleetmanager@isep.ipp.pt',
  password: 'GoodPass!1',
};

export const USER_CREDS = {
  user: simpleUserCreds,
  systemAdmin: systemAdminCreds,
  taskManager: taskManagerCreds,
  campusManager: campusManagerCreds,
  fleetManager: fleetManagerCreds,
};
