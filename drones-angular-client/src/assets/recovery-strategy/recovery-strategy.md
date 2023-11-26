**Backup Strategy Proposal for RoboDroneGo System**<br><br>

**Introduction**<br><br>

The backup strategy is designed to minimize Recovery Point Objective (RPO) and Work Recovery Time (WRT) for the RoboDroneGo system, which includes a node API, Angular application, and MongoDB Compass database. The backups will be stored in Microsoft Azure, following a specific retention policy. <br><br>

**Recovery Point Objective**

1 hour<br>
We accept the loss of data of one hour.<br><br>

**Work Recovery Time**

24 hours<br>
Meaning that restore the full functionality of a system after distribution takes a day.<br><br>

**Backup Schedule** <br><br>

Frequency:  
Hourly backups will be performed <br><br>

Retention:
Backups will be retained for a fixed period of 30 days.
The retention schedule ensures more frequent backups in the early days and reduces frequency as the data gets older.
Days 1-3: Every hourly backup will be retained.
Days 4-14: Every 3rd hourly backup will be retained.
Days 15-30: Every 6th hourly backup will be retained. <br><br>

Each backup will be a complete offline copy, ensuring data integrity. <br><br>

Backups will be stored in Microsoft Azure, leveraging Azure's reliability and scalability. <br><br>

Disaster Recovery Plan:

In the event of a disaster, the system will be restored to a different Azure region.
Azure's "Restore to a different region" functionality will be utilized, creating a new App Service app in a different region.
In case of physical damage, we have one backup robot and one server in our warehouse. <br><br>

Azure-Specific Considerations:
App Service Resources: The impacted app will be restored to a new app in a different Azure region.
Prerequisites: No specific prerequisites are required, and disaster recovery mode is automatically enabled.
Configuration: The target app in the new region will be configured to mirror the impacted app.
Domain Remapping: Once ready, the custom domain will be remapped to point to the target app.<br><br>

Testing:

Regular testing of the disaster recovery process will be conducted to ensure the efficiency and effectiveness of the backup strategy.<br><br>

By implementing this backup strategy, RoboDroneGo aims to ensure a high level of data availability and system operability, minimizing the impact of any potential disruptions and contributing to the overall business continuity objectives.
