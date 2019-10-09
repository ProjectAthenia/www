// import any ports you need
import athenia from './athenia';
import appPorts from './app';

// then run your ports in this function
export default function (app, storageKey) {
    athenia(app, storageKey);
    appPorts(app, storageKey);
}