/* @flow */

export type ASUserId = string;

export type ASUserGroup = {
  tag: 'Group';
  groupMembers: Array<ASUserId>;
  groupAdmins: Array<ASUserId>;
  groupName: string;
};

export type EntityGroup = {
  tag: 'EntityGroup';
  contents: ASUserGroup;
};

export type EntityUser = {
  tag: 'EntityUser';
  contents: ASUserId;
};

export type ASEntity = EntityGroup | EntityUser;

export type Blacklist = {
  tag: 'Blacklist';
  contents: Array<ASEntity>;
};

export type Whitelist = {
  tag: 'Whitelist';
  contents: Array<ASEntity>;
};

export type ASPermissions = Blacklist | Whitelist;
