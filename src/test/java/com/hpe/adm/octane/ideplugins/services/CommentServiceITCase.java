package com.hpe.adm.octane.ideplugins.services;


import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.junit.Assert;
import org.junit.Test;

import java.util.Collection;
import java.util.UUID;

public class CommentServiceITCase extends IntegrationTestBase {

    @Inject
    private CommentService commentService;

    @Inject
    private EntityService entityService;

    @Test
    public void testCommentService(){
        Collection<EntityModel> userStories = entityService.findEntities(Entity.USER_STORY);
        if(userStories.size()>0){
            EntityModel userStory = userStories.iterator().next();

            //Add a random comment
            String commentText =  "Test comment" + UUID.randomUUID().toString();
            commentService.postComment(userStory, commentText);

            //Check if there
            Collection<EntityModel> comments = commentService.getComments(userStory);

            //Check if comment was posted
            EntityModel lastComment = comments.iterator().next();
            String lastCommentText = lastComment.getValue("text").getValue().toString();
            Assert.assertTrue(lastCommentText!=null && lastCommentText.contains(commentText));

            commentService.deleteComment(lastComment.getValue("id").getValue().toString());

            comments = commentService.getComments(userStory);

            //Check if comment was deleted
            Assert.assertTrue(comments
                    .stream()
                    .map(comment -> comment.getValue("text").getValue().toString())
                    .filter(text -> text != null)
                    .noneMatch(text -> text.contains(commentText))
            );

        }
    }

}
