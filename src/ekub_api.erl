-module(ekub_api).

-export([endpoint/2]).

-type action() :: create | create_eviction | patch | replace |
                  delete | delete_collection |
                  read | list | list_all_ns |
                  watch | watch_list | watch_list_all_ns |
                  patch_status | read_status | replace_status |
                  exec | read_log.

-type object() :: cronjob | daemonset | deployment | job | pod |
                  replicaset | replicationcontroller | statefulset |
                  endpoint | ingress | service |
                  configmap | secret | persistentvolumeclaim |
                  storageclass | volumeattachment.

-type endpoint() :: HttpPath :: string().

-spec endpoint(Action :: action(), Object :: object()) ->
    Endpoint :: endpoint().

endpoint(Action, Object) -> case Object of
    cronjob -> case Action of
        create -> "/apis/batch/v1beta1/namespaces/~s/cronjobs";
        patch -> "/apis/batch/v1beta1/namespaces/~s/cronjobs/~s";
        replace -> "/apis/batch/v1beta1/namespaces/~s/cronjobs/~s";
        delete -> "/apis/batch/v1beta1/namespaces/~s/cronjobs/~s";
        delete_collection -> "/apis/batch/v1beta1/namespaces/~s/cronjobs";

        read -> "/apis/batch/v1beta1/namespaces/~s/cronjobs/~s";
        list -> "/apis/batch/v1beta1/namespaces/~s/cronjobs";
        list_all_ns -> "/apis/batch/v1beta1/cronjobs";

        watch -> "/apis/batch/v1beta1/watch/namespaces/~s/cronjobs/~s";
        watch_list -> "/apis/batch/v1beta1/watch/namespaces/~s/cronjobs";
        watch_list_all_ns -> "/apis/batch/v1beta1/watch/cronjobs";

        patch_status -> "/apis/batch/v1beta1/namespaces/~s/cronjobs/~s/status";
        read_status -> "/apis/batch/v1beta1/namespaces/~s/cronjobs/~s/status";
        replace_status -> "/apis/batch/v1beta1/namespaces/~s/cronjobs/~s/status"
    end;
    daemonset -> case Action of
        create -> "/apis/apps/v1/namespaces/~s/daemonsets";
        patch -> "/apis/apps/v1/namespaces/~s/daemonsets/~s";
        replace -> "/apis/apps/v1/namespaces/~s/daemonsets/~s";
        delete -> "/apis/apps/v1/namespaces/~s/daemonsets/~s";
        delete_collection -> "/apis/apps/v1/namespaces/~s/daemonsets";

        read -> "/apis/apps/v1/namespaces/~s/daemonsets/~s";
        list -> "/apis/apps/v1/namespaces/~s/daemonsets";
        list_all_ns -> "/apis/apps/v1/daemonsets";

        watch -> "/apis/apps/v1/watch/namespaces/~s/daemonsets/~s";
        watch_list -> "/apis/apps/v1/watch/namespaces/~s/daemonsets";
        watch_list_all_ns -> "/apis/apps/v1/watch/daemonsets";

        patch_status -> "/apis/apps/v1/namespaces/~s/daemonsets/~s/status";
        read_status -> "/apis/apps/v1/namespaces/~s/daemonsets/~s/status";
        replace_status -> "/apis/apps/v1/namespaces/~s/daemonsets/~s/status"
    end;
    deployment -> case Action of
        create -> "/apis/apps/v1/namespaces/~s/deployments";
        patch -> "/apis/apps/v1/namespaces/~s/deployments/~s";
        replace -> "/apis/apps/v1/namespaces/~s/deployments/~s";
        delete -> "/apis/apps/v1/namespaces/~s/deployments/~s";
        delete_collection -> "/apis/apps/v1/namespaces/~s/deployments";

        read -> "/apis/apps/v1/namespaces/~s/deployments/~s";
        list -> "/apis/apps/v1/namespaces/~s/deployments";
        list_all_ns -> "/apis/apps/v1/deployments";

        watch -> "/apis/apps/v1/watch/namespaces/~s/deployments/~s";
        watch_list -> "/apis/apps/v1/watch/namespaces/~s/deployments";
        watch_list_all_ns -> "/apis/apps/v1/watch/deployments";

        patch_status -> "/apis/apps/v1/namespaces/~s/deployments/~s/status";
        read_status -> "/apis/apps/v1/namespaces/~s/deployments/~s/status";
        replace_status -> "/apis/apps/v1/namespaces/~s/deployments/~s/status";

        read_scale -> "/apis/apps/v1/namespaces/~s/deployments/~s/scale";
        replace_scale -> "/apis/apps/v1/namespaces/~s/deployments/~s/scale";
        patch_scale -> "/apis/apps/v1/namespaces/~s/deployments/~s/scale"
    end;
    jobs -> case Action of
        create -> "/apis/batch/v1/namespaces/~s/jobs";
        patch -> "/apis/batch/v1/namespaces/~s/jobs/~s";
        replace -> "/apis/batch/v1/namespaces/~s/jobs/~s";
        delete -> "/apis/batch/v1/namespaces/~s/jobs/~s";
        delete_collection -> "/apis/batch/v1/namespaces/~s/jobs";

        read -> "/apis/batch/v1/namespaces/~s/jobs/~s";
        list -> "/apis/batch/v1/namespaces/~s/jobs";
        list_all_ns -> "/apis/batch/v1/jobs";

        watch -> "/apis/batch/v1/watch/namespaces/~s/jobs/~s";
        watch_list -> "/apis/batch/v1/watch/namespaces/~s/jobs";
        watch_list_all_ns -> "/apis/batch/v1/watch/jobs";

        patch_status -> "/apis/batch/v1/namespaces/~s/jobs/~s/status";
        read_status -> "/apis/batch/v1/namespaces/~s/jobs/~s/status";
        replace_status -> "/apis/batch/v1/namespaces/~s/jobs/~s/status"
    end;
    pod -> case Action of
        create -> "/api/v1/namespaces/~s/pods/~s";
        create_eviction -> "/api/v1/namespaces/~s/pods/~s/eviction";
        patch -> "/api/v1/namespaces/~s/pods/~s";
        replace -> "/api/v1/namespaces/~s/pods/~s";
        delete -> "/api/v1/namespaces/~s/pods/~s";
        delete_collection -> "/api/v1/namespaces/~s/pods";

        read -> "/api/v1/namespaces/~s/pods/~s";
        list -> "/api/v1/namespaces/~s/pods";
        list_all_ns -> "/api/v1/pods";

        watch -> "/api/v1/watch/namespaces/~s/pods/~s";
        watch_list -> "/api/v1/watch/namespaces/~s/pods";
        watch_list_all_ns -> "/api/v1/watch/pods";

        patch_status -> "/api/v1/namespaces/~s/pods/~s/status";
        read_status -> "/api/v1/namespaces/~s/pods/~s/status";
        replace_status -> "/api/v1/namespaces/~s/pods/~s/status";

        read_log -> "/api/v1/namespaces/~s/pods/~s/log";
        exec ->  "/api/v1/namespaces/~s/pods/~s/exec"
    end;
    statefulset -> case Action of
        create -> "/apis/apps/v1/namespaces/~s/statefulsets";
        patch -> "/apis/apps/v1/namespaces/~s/statefulsets/~s";
        replace -> "/apis/apps/v1/namespaces/~s/statefulsets/~s";
        delete -> "/apis/apps/v1/namespaces/~s/statefulsets/~s";
        delete_collection -> "/apis/apps/v1/namespaces/~s/statefulsets";

        read -> "/apis/apps/v1/namespaces/~s/statefulsets/~s";
        list -> "/apis/apps/v1/namespaces/~s/statefulsets";
        list_all_ns -> "/apis/apps/v1/statefulsets";

        watch -> "/apis/apps/v1/watch/namespaces/~s/statefulsets/~s";
        watch_list -> "/apis/apps/v1/watch/namespaces/~s/statefulsets";
        watch_list_all_ns -> "/apis/apps/v1/watch/statefulsets";

        patch_status -> "/apis/apps/v1/namespaces/~s/statefulsets/~s/status";
        read_status -> "/apis/apps/v1/namespaces/~s/statefulsets/~s/status";
        replace_status -> "/apis/apps/v1/namespaces/~s/statefulsets/~s/status";

        read_scale -> "/apis/apps/v1/namespaces/~s/statefulsets/~s/scale";
        replace_scale -> "/apis/apps/v1/namespaces/~s/statefulsets/~s/scale";
        patch_scale -> "/apis/apps/v1/namespaces/~s/statefulsets/~s/scale"
    end;
    endpoint -> case Action of
        create -> "/apis/v1/namespaces/~s/endpoints";
        patch -> "/apis/v1/namespaces/~s/endpoints/~s";
        replace -> "/apis/v1/namespaces/~s/endpoints/~s";
        delete -> "/apis/v1/namespaces/~s/endpoints/~s";
        delete_collection -> "/apis/v1/namespaces/~s/endpoints";

        read -> "/apis/v1/namespaces/~s/endpoints/~s";
        list -> "/apis/v1/namespaces/~s/endpoints";
        list_all_ns -> "/apis/v1/endpoints";

        watch -> "/apis/v1/watch/namespaces/~s/endpoints/~s";
        watch_list -> "/apis/v1/watch/namespaces/~s/endpoints";
        watch_list_all_ns -> "/apis/v1/watch/endpoints"
    end;
    ingress -> case Action of
        create -> "/apis/extensions/v1beta1/namespaces/~s/ingresses";
        patch -> "/apis/extensions/v1beta1/namespaces/~s/ingresses/~s";
        replace -> "/apis/extensions/v1beta1/namespaces/~s/ingresses/~s";
        delete -> "/apis/extensions/v1beta1/namespaces/~s/ingresses/~s";
        delete_collection -> "/apis/extensions/v1beta1/namespaces/~s/ingresses";

        read -> "/apis/extensions/v1beta1/namespaces/~s/ingresses/~s";
        list -> "/apis/extensions/v1beta1/namespaces/~s/ingresses";
        list_all_ns -> "/apis/extensions/v1beta1/ingresses";

        watch -> "/apis/extensions/v1beta1/watch/namespaces/~s/ingresses/~s";
        watch_list -> "/apis/extensions/v1beta1/watch/namespaces/~s/ingresses";
        watch_list_all_ns -> "/apis/extensions/v1beta1/watch/ingresses";

        patch_status ->
            "/apis/extensions/v1beta1/namespaces/~s/ingresses/~s/status";
        read_status ->
            "/apis/extensions/v1beta1/namespaces/~s/ingresses/~s/status";
        replace_status ->
            "/apis/extensions/v1beta1/namespaces/~s/ingresses/~s/status"
    end;
    service -> case Action of
        create -> "/api/v1/namespaces/~s/services";
        patch -> "/api/v1/namespaces/~s/services/~s";
        replace -> "/api/v1/namespaces/~s/services/~s";
        delete -> "/api/v1/namespaces/~s/services/~s";

        read -> "/api/v1/namespaces/~s/services/~s";
        list -> "/api/v1/namespaces/~s/services";
        list_all_ns -> "/api/v1/services";

        watch -> "/api/v1/watch/namespaces/~s/services/~s";
        watch_list -> "/api/v1/watch/namespaces/~s/services";
        watch_list_all_ns -> "/api/v1/watch/services";

        patch_status -> "/api/v1/namespaces/~s/services/~s/status";
        read_status -> "/api/v1/namespaces/~s/services/~s/status";
        replace_status -> "/api/v1/namespaces/~s/services/~s/status"
    end
end.
