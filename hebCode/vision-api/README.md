# Vision Api

This is an HTTP REST API for a service that ingests user images, analyzes them for object
detection and returns the enhanced content. There are three endpoints,one is to get all 
images and also for taking only imagesthat have the detected objects specified in the 
query parameter. The second endpoint is for getting image details for a specific image. 
The last endpoint is for sending JSON request body including an image file or URL, an 
optional label for the image, and an optional field to enable object detection. 
This endpoint returnsJSON response body including the image data, its label, and its 
identifier provided by the persistent data store, and any objects detected.

